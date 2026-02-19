use std::path::{Path, PathBuf};

use crate::build::linux::RelocEntry;

pub fn write_executable_from_sections(
    out_dir: &Path,
    proj_name: &str,
    _target: &str,
    text: &mut Vec<u8>,
    rodata: &Vec<u8>,
    string_positions: &Vec<usize>,
    helper_pos: &std::collections::HashMap<String, usize>,
    reloc_entries: &Vec<RelocEntry>,
    func_label_map: &std::collections::HashMap<String, usize>,
    label_positions: &std::collections::HashMap<usize, usize>,
    argc_ro_offset: usize,
    argv_ro_offset: usize,
) -> Result<PathBuf, String> {
    const IMAGE_BASE: u64 = 0x1400_0000_00u64;
    const SECTION_ALIGNMENT: u32 = 0x1000;
    const FILE_ALIGNMENT: u32 = 0x200;
    const HEADER_NT_OFFSET: u32 = 0x80;

    for r in reloc_entries.iter() {
        let sym_name = r
            .sym_name
            .as_ref()
            .ok_or_else(|| "missing symbol name for relocation".to_string())?;
        let sym = String::from_utf8_lossy(sym_name).into_owned();

        let sym_pos_opt: Option<u64> = if let Some(p) = helper_pos.get(sym.as_str()) {
            Some((*p) as u64)
        } else if let Some(stripped) = sym.strip_prefix(".str.") {
            if let Ok(idx) = stripped.parse::<usize>() {
                if idx < string_positions.len() {
                    Some((text.len() + string_positions[idx]) as u64)
                } else {
                    None
                }
            } else {
                None
            }
        } else if sym == "__argc" {
            Some((text.len() + argc_ro_offset) as u64)
        } else if sym == "__argv" {
            Some((text.len() + argv_ro_offset) as u64)
        } else if let Some(lbl) = func_label_map.get(&sym) {
            label_positions.get(lbl).cloned().map(|v| v as u64)
        } else if sym == "_start" {
            Some(0u64)
        } else {
            None
        };

        let pos_in_text =
            sym_pos_opt.ok_or_else(|| format!("undefined symbol in relocation: {}", sym))?;

        let off = r.offset;
        if off + 8 > text.len() {
            return Err(format!(
                "relocation offset out of range: {} > {}",
                off + 8,
                text.len()
            ));
        }

        let le = (pos_in_text as u64).to_le_bytes();
        text[off..off + 8].copy_from_slice(&le);
    }

    fn align_up(v: u64, a: u64) -> u64 {
        ((v + a - 1) / a) * a
    }

    let text_vsize = text.len() as u64;
    let rdata_vsize = rodata.len() as u64;

    let text_raw_size = align_up(text_vsize, FILE_ALIGNMENT as u64) as u32;
    let rdata_raw_size = align_up(rdata_vsize, FILE_ALIGNMENT as u64) as u32;

    let text_rva = SECTION_ALIGNMENT as u64;
    let rdata_rva = text_rva + align_up(text_vsize, SECTION_ALIGNMENT as u64);

    let size_of_image = (rdata_rva + align_up(rdata_vsize, SECTION_ALIGNMENT as u64)) as u32;
    let size_of_headers = align_up(
        (HEADER_NT_OFFSET as u64) + 4 + 20 + 0xF0 + (2 * 40),
        FILE_ALIGNMENT as u64,
    ) as u32;

    for r in reloc_entries.iter() {
        let off = r.offset;
        let stored = u64::from_le_bytes(text[off..off + 8].try_into().unwrap());

        let final_va: u64 = if stored < (text.len() as u64) {
            IMAGE_BASE + text_rva + stored
        } else {
            let ro_off = stored - (text.len() as u64);
            IMAGE_BASE + rdata_rva + ro_off
        };
        let le = final_va.to_le_bytes();
        text[off..off + 8].copy_from_slice(&le);
    }

    let mut pe: Vec<u8> = Vec::new();

    pe.extend_from_slice(&[0x4D, 0x5A]);
    pe.resize(0x3C, 0);
    pe.extend_from_slice(&(HEADER_NT_OFFSET as u32).to_le_bytes());

    if pe.len() < (HEADER_NT_OFFSET as usize) {
        pe.resize(HEADER_NT_OFFSET as usize, 0);
    }

    pe.extend_from_slice(&[b'P', b'E', 0, 0]);

    pe.extend_from_slice(&0x8664u16.to_le_bytes());
    pe.extend_from_slice(&2u16.to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&(0xF0u16).to_le_bytes());
    pe.extend_from_slice(&(0x0022u16).to_le_bytes());

    pe.extend_from_slice(&(0x20Bu16).to_le_bytes());
    pe.push(0);
    pe.push(0);

    pe.extend_from_slice(&(text_raw_size as u32).to_le_bytes());
    pe.extend_from_slice(&(rdata_raw_size as u32).to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());

    let entry_rva = text_rva as u32;
    pe.extend_from_slice(&entry_rva.to_le_bytes());

    pe.extend_from_slice(&(text_rva as u32).to_le_bytes());

    pe.extend_from_slice(&IMAGE_BASE.to_le_bytes());

    pe.extend_from_slice(&(SECTION_ALIGNMENT as u32).to_le_bytes());
    pe.extend_from_slice(&(FILE_ALIGNMENT as u32).to_le_bytes());

    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&4u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());

    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&(size_of_image as u32).to_le_bytes());
    pe.extend_from_slice(&(size_of_headers as u32).to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&3u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());

    pe.extend_from_slice(&0x100000u64.to_le_bytes());
    pe.extend_from_slice(&0x1000u64.to_le_bytes());
    pe.extend_from_slice(&0x100000u64.to_le_bytes());
    pe.extend_from_slice(&0x1000u64.to_le_bytes());

    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&16u32.to_le_bytes());

    pe.extend_from_slice(&[0u8; 16 * 8]);

    let mut sh_name = [0u8; 8];
    sh_name[..5].copy_from_slice(b".text");
    pe.extend_from_slice(&sh_name);
    pe.extend_from_slice(&(text_vsize as u32).to_le_bytes());
    pe.extend_from_slice(&(text_rva as u32).to_le_bytes());
    pe.extend_from_slice(&(text_raw_size as u32).to_le_bytes());
    pe.extend_from_slice(&(size_of_headers as u32).to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&(0x60000020u32).to_le_bytes());

    let mut sh_name2 = [0u8; 8];
    sh_name2[..6].copy_from_slice(b".rdata");
    pe.extend_from_slice(&sh_name2);
    pe.extend_from_slice(&(rdata_vsize as u32).to_le_bytes());
    pe.extend_from_slice(&(rdata_rva as u32).to_le_bytes());
    pe.extend_from_slice(&(rdata_raw_size as u32).to_le_bytes());
    pe.extend_from_slice(&((size_of_headers as u32) + text_raw_size).to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u32.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&0u16.to_le_bytes());
    pe.extend_from_slice(&(0x40000040u32).to_le_bytes());

    if (pe.len() as u32) < size_of_headers {
        pe.resize(size_of_headers as usize, 0);
    }

    let mut file_bytes: Vec<u8> = pe;
    file_bytes.extend_from_slice(&text);

    let pad_text = (FILE_ALIGNMENT as usize) - (text.len() % FILE_ALIGNMENT as usize);
    if pad_text != FILE_ALIGNMENT as usize {
        file_bytes.resize(file_bytes.len() + pad_text, 0);
    }

    file_bytes.extend_from_slice(rodata);
    let pad_rdata = (FILE_ALIGNMENT as usize) - (rodata.len() % FILE_ALIGNMENT as usize);
    if pad_rdata != FILE_ALIGNMENT as usize {
        file_bytes.resize(file_bytes.len() + pad_rdata, 0);
    }

    std::fs::create_dir_all(out_dir).map_err(|e| format!("failed to create out dir: {}", e))?;
    let exe_name = format!(
        "{}{}",
        proj_name,
        crate::build::get_executable_extension("x86_64-windows")
    );
    let exe_path = out_dir.join(exe_name);
    std::fs::write(&exe_path, &file_bytes).map_err(|e| format!("failed to write exe: {}", e))?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&exe_path)
            .map_err(|e| format!("stat failed: {}", e))?
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&exe_path, perms)
            .map_err(|e| format!("set_permissions failed: {}", e))?;
    }

    Ok(exe_path)
}

#[cfg(test)]
#[cfg(not(windows))]
mod tests {
    use crate::build::compile_project;
    use crate::libraries::runtime_helpers;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn compile_windows_produces_pe_on_unix() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"hi\")/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let data = std::fs::read(&exe).expect("read exe");

        assert_eq!(&data[0..2], b"MZ");

        let e_lfanew = u32::from_le_bytes(data[0x3C..0x40].try_into().unwrap()) as usize;
        assert!(e_lfanew + 4 <= data.len());
        assert_eq!(&data[e_lfanew..e_lfanew + 4], b"PE\0\0");
    }

    #[test]
    fn compile_windows_embeds_runtime_helpers_on_unix() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let _ = args()/array\n    println(\"ok\")/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let data = std::fs::read(&exe).expect("read exe");

        let helpers = runtime_helpers::get_helpers();
        let helper = helpers.get("len").expect("helper missing");

        assert!(data.windows(helper.len()).any(|w| w == helper.as_slice()));
    }
}

#[cfg(test)]
#[cfg(windows)]
mod run_tests {
    use crate::build::compile_project;
    use std::fs;
    use std::process::Command;
    use tempfile::tempdir;

    fn run_exe(exe: &std::path::Path) -> std::io::Result<std::process::Output> {
        Command::new(exe).output()
    }

    #[test]
    fn compile_simple_println_and_run_windows() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"hello\")/unit\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "hello\n");
    }

    #[test]
    fn compile_args_and_run_windows() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        std::fs::create_dir_all(pd.join("src")).expect("mkdir");
        std::fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"args {} {}\", args()[0], args()[1])/unit\nfn/",
        )
        .expect("write src");
        std::fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let output = Command::new(&exe)
            .arg("one")
            .arg("two")
            .output()
            .expect("run exe");
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "args one two\n");
    }
}
