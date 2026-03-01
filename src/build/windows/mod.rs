use std::path::{Path, PathBuf};

use crate::build::linux::RelocEntry;

#[allow(clippy::too_many_arguments)]
pub fn write_executable_from_sections(
    out_dir: &Path,
    proj_name: &str,
    _target: &str,
    text: &mut [u8],
    rodata: &[u8],
    string_positions: &[usize],
    helper_pos: &std::collections::HashMap<String, usize>,
    reloc_entries: &[RelocEntry],
    func_label_map: &std::collections::HashMap<String, usize>,
    label_positions: &std::collections::HashMap<usize, usize>,
    argc_ro_offset: usize,
    argv_ro_offset: usize,
    argv_store_ro_offset: usize,
) -> Result<PathBuf, String> {
    const IMAGE_BASE: u64 = 0x0014_0000_0000_u64;
    const SECTION_ALIGNMENT: u32 = 0x1000;
    const FILE_ALIGNMENT: u32 = 0x200;
    const HEADER_NT_OFFSET: u32 = 0x80;

    let text_rva = SECTION_ALIGNMENT as u64;

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
        } else if let Some(lbl) = func_label_map.get(&sym) {
            label_positions.get(lbl).cloned().map(|v| v as u64)
        } else if sym == "_start" {
            Some(0u64)
        } else {
            None
        };

        let pos_in_text_opt = sym_pos_opt;
        if pos_in_text_opt.is_none() {
            continue;
        }
        let pos_in_text = pos_in_text_opt.unwrap();

        let off = r.offset;
        if off + 8 > text.len() {
            return Err(format!(
                "relocation offset out of range: {} > {}",
                off + 8,
                text.len()
            ));
        }

        let va = IMAGE_BASE + text_rva + pos_in_text;
        let le = va.to_le_bytes();
        text[off..off + 8].copy_from_slice(&le);
    }

    fn align_up(v: u64, a: u64) -> u64 {
        v.div_ceil(a) * a
    }

    let mut import_funcs: Vec<String> = Vec::new();
    for r in reloc_entries.iter() {
        if let Some(sym_name) = &r.sym_name {
            let s = String::from_utf8_lossy(sym_name).into_owned();

            if !helper_pos.contains_key(s.as_str())
                && !s.starts_with(".str.")
                && s != "__argc"
                && s != "__argv"
                && s != "__argv_store"
                && s != "_start"
                && !func_label_map.contains_key(&s)
                && !import_funcs.contains(&s)
            {
                import_funcs.push(s);
            }
        }
    }

    let mut import_data: Vec<u8> = Vec::new();
    let mut import_by_name_offsets: Vec<u32> = Vec::new();

    if !import_funcs.is_empty() {
        let dll_name = b"kernel32.dll\0";

        let _ilt_offset_in_block = import_data.len();
        for _ in &import_funcs {
            import_data.extend_from_slice(&0u64.to_le_bytes());
        }
        import_data.extend_from_slice(&0u64.to_le_bytes());

        let _iat_offset_in_block = import_data.len();
        for _ in &import_funcs {
            import_data.extend_from_slice(&0u64.to_le_bytes());
        }
        import_data.extend_from_slice(&0u64.to_le_bytes());

        for f in &import_funcs {
            let name_rva_in_block = import_data.len() as u32;
            import_data.extend_from_slice(&0u16.to_le_bytes());
            import_data.extend_from_slice(f.as_bytes());
            import_data.push(0);
            import_by_name_offsets.push(name_rva_in_block);
        }

        let _dll_name_rva_in_block = import_data.len() as u32;
        import_data.extend_from_slice(dll_name);

        let _import_descriptor_offset = import_data.len();
        import_data.extend_from_slice(&0u32.to_le_bytes());
        import_data.extend_from_slice(&0u32.to_le_bytes());
        import_data.extend_from_slice(&0u32.to_le_bytes());
        import_data.extend_from_slice(&0u32.to_le_bytes());
        import_data.extend_from_slice(&0u32.to_le_bytes());

        import_data.extend_from_slice(&[0u8; 20]);
    }

    let text_vsize = text.len() as u64;

    let rodata_len = rodata.len();
    let pad_rdata = if rodata_len.is_multiple_of(FILE_ALIGNMENT as usize) {
        0
    } else {
        (FILE_ALIGNMENT as usize) - (rodata_len % (FILE_ALIGNMENT as usize))
    };
    let rdata_vsize = (rodata_len + pad_rdata + import_data.len()) as u64;

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
        let sym_name = r
            .sym_name
            .as_ref()
            .map(|s| String::from_utf8_lossy(s).into_owned());

        if let Some(sym) = sym_name {
            if let Some(p) = helper_pos.get(sym.as_str()) {
                let pos_in_text = *p as u64;
                let va = IMAGE_BASE + text_rva + pos_in_text;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if let Some(stripped) = sym.strip_prefix(".str.")
                && let Ok(idx) = stripped.parse::<usize>()
                && idx < string_positions.len()
            {
                let pos_in_rdata = string_positions[idx] as u64;
                let va = IMAGE_BASE + rdata_rva + pos_in_rdata;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if sym == "__argc" {
                let pos_in_rdata = argc_ro_offset as u64;
                let va = IMAGE_BASE + rdata_rva + pos_in_rdata;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if sym == "__argv" {
                let pos_in_rdata = argv_ro_offset as u64;
                let va = IMAGE_BASE + rdata_rva + pos_in_rdata;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if sym == "__argv_store" {
                let pos_in_rdata = argv_store_ro_offset as u64;
                let va = IMAGE_BASE + rdata_rva + pos_in_rdata;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if let Some(lbl) = func_label_map.get(&sym)
                && let Some(v) = label_positions.get(lbl)
            {
                let pos_in_text = *v as u64;
                let va = IMAGE_BASE + text_rva + pos_in_text;
                let le = va.to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
            if sym == "_start" {
                let le = (0u64).to_le_bytes();
                text[off..off + 8].copy_from_slice(&le);
                continue;
            }
        }
    }

    let mut pe: Vec<u8> = Vec::new();

    pe.extend_from_slice(&[0x4D, 0x5A]);
    pe.resize(0x3C, 0);
    pe.extend_from_slice(&HEADER_NT_OFFSET.to_le_bytes());

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

    pe.extend_from_slice(&SECTION_ALIGNMENT.to_le_bytes());
    pe.extend_from_slice(&FILE_ALIGNMENT.to_le_bytes());

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

    let data_directory_offset = pe.len();
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

    pe.extend_from_slice(&(0xC0000040u32).to_le_bytes());

    if (pe.len() as u32) < size_of_headers {
        pe.resize(size_of_headers as usize, 0);
    }

    let mut file_bytes: Vec<u8> = pe.clone();
    file_bytes.extend_from_slice(text);

    let pad_text = (FILE_ALIGNMENT as usize) - (text.len() % FILE_ALIGNMENT as usize);
    if pad_text != FILE_ALIGNMENT as usize {
        file_bytes.resize(file_bytes.len() + pad_text, 0);
    }

    let rodata_start = file_bytes.len();
    file_bytes.extend_from_slice(rodata);
    if pad_rdata != 0 {
        file_bytes.resize(file_bytes.len() + pad_rdata, 0);
    }

    if !import_funcs.is_empty() {
        let import_block_offset_in_file = file_bytes.len();
        file_bytes.extend_from_slice(&import_data);

        let import_block_rva = rdata_rva + ((import_block_offset_in_file - rodata_start) as u64);

        let mut cursor = import_block_offset_in_file;

        let n = import_funcs.len();

        let ilt_file_off = cursor;
        cursor += (n + 1) * 8;

        let iat_file_off = cursor;
        cursor += (n + 1) * 8;

        let _name_entries_file_off = cursor;

        let mut name_rvas: Vec<u32> = Vec::new();
        for _i in 0..n {
            let mut pos = cursor;

            pos += 2;

            while file_bytes[pos] != 0 {
                pos += 1;
            }
            let name_rva =
                (import_block_rva as u32) + (cursor as u32 - import_block_offset_in_file as u32);
            name_rvas.push(name_rva);

            cursor = pos + 1;
        }

        let dll_name_file_off = cursor;

        while file_bytes[cursor] != 0 {
            cursor += 1;
        }
        cursor += 1;

        let import_descriptor_file_off = cursor;

        for (i, _f) in import_funcs.iter().enumerate() {
            let ilt_entry_off = ilt_file_off + (i * 8);
            let iat_entry_off = iat_file_off + (i * 8);
            let by_name_rva = name_rvas[i] as u64;

            let buf = (by_name_rva).to_le_bytes();
            file_bytes[ilt_entry_off..(8 + ilt_entry_off)].copy_from_slice(&buf);
            file_bytes[iat_entry_off..(8 + iat_entry_off)].copy_from_slice(&buf);
        }

        let ilt_rva =
            import_block_rva as u32 + (ilt_file_off as u32 - import_block_offset_in_file as u32);
        let iat_rva =
            import_block_rva as u32 + (iat_file_off as u32 - import_block_offset_in_file as u32);
        let dll_name_rva = import_block_rva as u32
            + (dll_name_file_off as u32 - import_block_offset_in_file as u32);

        file_bytes[import_descriptor_file_off..import_descriptor_file_off + 4]
            .copy_from_slice(&ilt_rva.to_le_bytes());

        file_bytes[import_descriptor_file_off + 12..import_descriptor_file_off + 16]
            .copy_from_slice(&dll_name_rva.to_le_bytes());
        file_bytes[import_descriptor_file_off + 16..import_descriptor_file_off + 20]
            .copy_from_slice(&iat_rva.to_le_bytes());

        let import_dir_rva =
            (rdata_rva + (import_descriptor_file_off as u64 - rodata_start as u64)) as u32;
        let import_dir_size = ((cursor + 20) - import_descriptor_file_off) as u32;

        let imp_off = data_directory_offset + 8;

        for buf in [&mut pe, &mut file_bytes].iter_mut() {
            buf[imp_off..imp_off + 4].copy_from_slice(&import_dir_rva.to_le_bytes());
            buf[imp_off + 4..imp_off + 8].copy_from_slice(&import_dir_size.to_le_bytes());
        }

        for r in reloc_entries.iter() {
            if let Some(sym_name) = &r.sym_name {
                let s = String::from_utf8_lossy(sym_name).into_owned();
                if import_funcs.contains(&s) {
                    let off = r.offset;
                    if off >= 2 {
                        text[off - 2] = 0x48;
                        text[off - 1] = 0xA1;
                    }

                    let idx = import_funcs.iter().position(|x| x == &s).unwrap();
                    let iat_entry_va = IMAGE_BASE
                        + import_block_rva
                        + (iat_file_off as u64 - import_block_offset_in_file as u64)
                        + (idx as u64 * 8);
                    let le = iat_entry_va.to_le_bytes();
                    text[off..off + 8].copy_from_slice(&le);

                    let file_off = pe.len() + off;
                    if file_off >= 2 {
                        file_bytes[file_off - 2] = 0x48;
                        file_bytes[file_off - 1] = 0xA1;
                    }
                    file_bytes[file_off..file_off + 8].copy_from_slice(&le);
                } else if let Some(helper_offset) = helper_pos.get(s.as_str()) {
                    let off = r.offset;
                    let helper_va = IMAGE_BASE + text_rva + (*helper_offset as u64);
                    let le = helper_va.to_le_bytes();
                    text[off..off + 8].copy_from_slice(&le);

                    let file_off = pe.len() + off;
                    file_bytes[file_off..file_off + 8].copy_from_slice(&le);
                } else if s == "__argc" {
                    let off = r.offset;
                    let argc_va = IMAGE_BASE + rdata_rva + (argc_ro_offset as u64);
                    let le = argc_va.to_le_bytes();
                    text[off..off + 8].copy_from_slice(&le);

                    let file_off = pe.len() + off;
                    file_bytes[file_off..file_off + 8].copy_from_slice(&le);
                } else if s == "__argv" {
                    let off = r.offset;
                    let argv_va = IMAGE_BASE + rdata_rva + (argv_ro_offset as u64);
                    let le = argv_va.to_le_bytes();
                    text[off..off + 8].copy_from_slice(&le);

                    let file_off = pe.len() + off;
                    file_bytes[file_off..file_off + 8].copy_from_slice(&le);
                }
            }
        }
    }

    let expected_size =
        (size_of_headers as usize) + (text_raw_size as usize) + (rdata_raw_size as usize);
    if file_bytes.len() < expected_size {
        file_bytes.resize(expected_size, 0);
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
    use crate::libraries::runtime;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn compile_windows_produces_pe_on_unix() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"hi\")/\nfn/",
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
            "fn new main/\n    let _ = args()/array\n    println(\"ok\")/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let data = std::fs::read(&exe).expect("read exe");

        let helpers = runtime::get_helpers("x86_64-windows");

        let mut found_any = false;
        for (_name, helper) in helpers.iter() {
            if data.windows(helper.len()).any(|w| w == helper.as_slice()) {
                found_any = true;
                break;
            }
        }
        assert!(found_any, "no runtime helper bytes embedded in exe");
    }

    #[test]
    fn compile_windows_args_path_embeds_args_helpers_on_unix() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"args {} {}\", args()[0], args()[1])/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let data = std::fs::read(&exe).expect("read exe");

        assert!(
            data.windows(b"GetCommandLineA".len())
                .any(|w| w == b"GetCommandLineA"),
            "GetCommandLineA import not found for args path"
        );
        assert!(
            !data
                .windows(b"__build_args_array".len())
                .any(|w| w == b"__build_args_array"),
            "__build_args_array leaked as unresolved import"
        );
        assert!(
            !data
                .windows(b"__cstr_to_string".len())
                .any(|w| w == b"__cstr_to_string"),
            "__cstr_to_string leaked as unresolved import"
        );
    }

    #[test]
    fn compile_windows_network_helpers_on_unix() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let s = net server new(8000)/i64\n    let _ = net_close(s)/i64\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");

        let data = std::fs::read(&exe).expect("read exe");

        let helpers = runtime::get_helpers("x86_64-windows");
        let net_helper = helpers
            .get("net_server_new")
            .expect("network helper missing");
        assert!(
            data.windows(net_helper.len())
                .any(|w| w == net_helper.as_slice())
        );
    }

    #[test]
    fn import_relocations_are_applied() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"bye\")/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");
        let data = fs::read(&exe).expect("read exe");

        let mut text_file_off = None;
        let mut text_rva = None;
        if let Some(pos) = data.windows(b".text".len()).position(|w| w == b".text") {
            text_file_off =
                Some(u32::from_le_bytes(data[pos + 20..pos + 24].try_into().unwrap()) as usize);
            text_rva = Some(u32::from_le_bytes(
                data[pos + 8 + 4..pos + 8 + 8].try_into().unwrap(),
            ));
        }
        let (text_off, _text_rva) = (
            text_file_off.expect("could not find .text header"),
            text_rva.expect("could not find .text rva"),
        );

        let mut found = false;
        let mut exit_imm = 0u64;
        for i in text_off..text_off + 0x100 {
            if i + 12 > data.len() {
                break;
            }
            if data[i] == 0x48
                && data[i + 1] == 0xA1
                && data[i + 10] == 0xFF
                && data[i + 11] == 0xD0
            {
                exit_imm = u64::from_le_bytes(data[i + 2..i + 10].try_into().unwrap());
                assert!(exit_imm != 0, "import relocation remained zero");
                found = true;
                break;
            }
        }
        assert!(found, "couldn't find exit call pattern");

        let mut rdata_rva = 0u32;
        let mut rdata_file = 0u32;
        if let Some(pos) = data.windows(b".rdata".len()).position(|w| w == b".rdata") {
            rdata_rva = u32::from_le_bytes(data[pos + 8 + 4..pos + 8 + 8].try_into().unwrap());

            rdata_file = u32::from_le_bytes(data[pos + 20..pos + 24].try_into().unwrap());
        }
        assert!(
            rdata_rva != 0 && rdata_file != 0,
            "could not parse .rdata header"
        );

        const IMAGE_BASE: u64 = 0x0014_0000_0000_u64;
        let rva = (exit_imm - IMAGE_BASE) as usize;
        let iat_off = rdata_file as usize + (rva - rdata_rva as usize);
        assert!(
            iat_off + 8 <= data.len(),
            "IAT offset out of range: computed 0x{:x} but file len 0x{:x} (exit_imm=0x{:x}, rdata_rva=0x{:x}, rdata_file=0x{:x})",
            iat_off + 8,
            data.len(),
            exit_imm,
            rdata_rva,
            rdata_file
        );
        assert!(
            data[iat_off..iat_off + 8].iter().any(|&b| b != 0),
            "IAT entry was zero"
        );

        for i in 0..data.len().saturating_sub(2) {
            if data[i] == 0x48 && data[i + 1] == 0xA3 {
                panic!("illegal direct store instruction left in binary");
            }
        }
    }

    #[test]
    fn compile_args_and_run_with_wine() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"args {} {}\", args()[0], args()[1])/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-windows"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-windows", None).expect("compile");

        let output = std::process::Command::new("wine")
            .arg(&exe)
            .arg("one")
            .arg("two")
            .output();

        if let Ok(output) = output {
            if !output.status.success() {
                eprintln!("Exit status: {:?}", output.status);
                eprintln!("Stdout: {}", String::from_utf8_lossy(&output.stdout));
                eprintln!("Stderr: {}", String::from_utf8_lossy(&output.stderr));
            }
            assert!(output.status.success(), "executable failed");
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                "args one two\n",
                "incorrect output"
            );
        } else {
            eprintln!("Wine not available, skipping execution test");
        }
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
            "fn new main/\n    println(\"hello\")/\nfn/",
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
            "fn new main/\n    println(\"args {} {}\", args()[0], args()[1])/\nfn/",
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
        if !output.status.success() {
            eprintln!("Exit status: {:?}", output.status);
            eprintln!("Stdout: {}", String::from_utf8_lossy(&output.stdout));
            eprintln!("Stderr: {}", String::from_utf8_lossy(&output.stderr));
        }
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "args one two\n");
    }
}
