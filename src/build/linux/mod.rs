use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub struct RelocEntry {
    pub offset: usize,
    pub sym_name: Option<Vec<u8>>,
    pub _is_call: bool,
}

#[allow(clippy::too_many_arguments)]
pub fn write_executable_from_sections(
    out_dir: &Path,
    proj_name: &str,
    target: &str,
    text: &mut [u8],
    rodata: &[u8],
    string_positions: &[usize],
    helper_pos: &HashMap<String, usize>,
    reloc_entries: &[RelocEntry],
    func_label_map: &HashMap<String, usize>,
    label_positions: &HashMap<usize, usize>,
    argc_ro_offset: usize,
    argv_ro_offset: usize,
) -> Result<PathBuf, String> {
    let base_vaddr: u64 = 0x400000;
    let file_text_off: usize = 0x1000;

    for r in reloc_entries.iter() {
        let sym_name = r
            .sym_name
            .as_ref()
            .ok_or_else(|| "missing symbol name for relocation".to_string())?;
        let sym = String::from_utf8_lossy(sym_name).into_owned();
        let sym_pos_opt = if let Some(p) = helper_pos.get(sym.as_str()) {
            Some(*p)
        } else if let Some(stripped) = sym.strip_prefix(".str.") {
            if let Ok(idx) = stripped.parse::<usize>() {
                if idx < string_positions.len() {
                    Some(text.len() + string_positions[idx])
                } else {
                    None
                }
            } else {
                None
            }
        } else if sym == "__argc" {
            Some(text.len() + argc_ro_offset)
        } else if sym == "__argv" {
            Some(text.len() + argv_ro_offset)
        } else if let Some(lbl) = func_label_map.get(&sym) {
            label_positions.get(lbl).cloned()
        } else if sym == "_start" {
            Some(0usize)
        } else {
            None
        };
        let pos = sym_pos_opt.ok_or_else(|| format!("undefined symbol in relocation: {}", sym))?;
        let addr = base_vaddr + (file_text_off as u64) + (pos as u64);
        let off = r.offset;
        let le = (addr).to_le_bytes();
        if off + 8 > text.len() {
            return Err(format!(
                "relocation offset out of range: {} > {}",
                off + 8,
                text.len()
            ));
        }
        text[off..off + 8].copy_from_slice(&le);
    }

    fn write_u16(buf: &mut Vec<u8>, v: u16) {
        buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u32(buf: &mut Vec<u8>, v: u32) {
        buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u64(buf: &mut Vec<u8>, v: u64) {
        buf.extend_from_slice(&v.to_le_bytes());
    }

    let mut elf: Vec<u8> = Vec::new();

    elf.extend_from_slice(&[0x7f, b'E', b'L', b'F']);
    elf.push(2);
    elf.push(1);
    elf.push(1);
    elf.extend_from_slice(&[0u8; 9]);
    write_u16(&mut elf, 2);
    write_u16(&mut elf, 0x3e);
    write_u32(&mut elf, 1);

    let entry_addr = base_vaddr + (file_text_off as u64);
    write_u64(&mut elf, entry_addr);

    let phoff = 64u64;
    write_u64(&mut elf, phoff);
    write_u64(&mut elf, 0);
    write_u32(&mut elf, 0);
    write_u16(&mut elf, 64);
    write_u16(&mut elf, 56);
    write_u16(&mut elf, 1);
    write_u16(&mut elf, 0);
    write_u16(&mut elf, 0);
    write_u16(&mut elf, 0);

    write_u32(&mut elf, 1);

    write_u32(&mut elf, 7);

    write_u64(&mut elf, file_text_off as u64);

    write_u64(&mut elf, base_vaddr + (file_text_off as u64));

    write_u64(&mut elf, base_vaddr + (file_text_off as u64));

    let filesz = (text.len() + rodata.len()) as u64;
    write_u64(&mut elf, filesz);
    write_u64(&mut elf, filesz);

    write_u64(&mut elf, 0x1000);

    if elf.len() > file_text_off {
        return Err("ELF headers too large".into());
    }
    let mut file_bytes: Vec<u8> = elf;
    file_bytes.resize(file_text_off, 0);

    file_bytes.extend_from_slice(text);
    file_bytes.extend_from_slice(rodata);

    let exe_name = format!("{}{}", proj_name, super::get_executable_extension(target));
    let exe_path = out_dir.join(&exe_name);
    {
        use std::io::Write;
        let mut f =
            std::fs::File::create(&exe_path).map_err(|e| format!("failed to create exe: {}", e))?;
        f.write_all(&file_bytes)
            .map_err(|e| format!("failed to write exe: {}", e))?;
        f.sync_all()
            .map_err(|e| format!("failed to sync exe: {}", e))?;

        drop(f);
    }

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
#[cfg(unix)]
mod tests {
    use crate::build::compile_project;
    use std::fs;
    use std::process::Command;
    use tempfile::tempdir;

    fn run_exe(exe: &std::path::Path) -> std::io::Result<std::process::Output> {
        let mut attempts = 0;
        const MAX_ATTEMPTS: u32 = 5;

        loop {
            match Command::new(exe).output() {
                Ok(output) => return Ok(output),
                Err(e) if e.raw_os_error() == Some(26) && attempts < MAX_ATTEMPTS => {
                    attempts += 1;
                    std::thread::sleep(std::time::Duration::from_millis(50));
                }
                Err(e) => return Err(e),
            }
        }
    }

    #[test]
    fn compile_simple_println_and_run() {
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
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "hello\n");
    }

    #[test]
    fn compile_linux_network_helpers() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let s = net server new(8080)/i64\n    let _ = net_close(s)/i64\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let data = std::fs::read(&exe).expect("read exe");

        let helpers = crate::libraries::runtime::get_helpers("x86_64-linux");
        let net_h = helpers.get("net_server_new").expect("missing net helper");
        assert!(data.windows(net_h.len()).any(|w| w == net_h.as_slice()));
    }

    #[test]
    fn compile_println_with_placeholder_and_expr() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"sum {} {}\", 2+3, 4)/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "sum 5 4\n");
    }

    #[test]
    fn compile_locals_and_arith() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let x = 2/i64\n    let y = 3/i64\n    println(\"sum {}\", x + y)/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(String::from_utf8_lossy(&out.stdout), "sum 5\n");
    }

    #[test]
    fn compile_locals_simple() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let x = 42/i64\n    println(\"after {}\", x)/\n    println(\"both {} {}\", 42, x)/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_test_exe"));

        let objdump = Command::new("objdump").arg("-d").arg(&exe).output().ok();
        if let Some(od) = objdump {
            println!("objdump stdout:\n{}", String::from_utf8_lossy(&od.stdout));
            println!("objdump stderr:\n{}", String::from_utf8_lossy(&od.stderr));
        }
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "after 42\nboth 42 42\n"
        );
    }

    #[test]
    fn compile_if_else_and_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    if 1 == 1/\n        println(\"yes\")/\n    else/\n        println(\"no\")/\n    if/\n    println(\"after\")/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "yes\nafter\n");
    }

    #[test]
    fn compile_while_and_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new main/\n    let i = 0/i64\n    while i < 3/\n        println(\"{}\", i)/\n        i = i + 1/\n    while/\n    println(\"done\")/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "0\n1\n2\ndone\n");
    }

    #[test]
    fn compile_short_circuit_and() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"res {}\", 0 == 1 && (1 / 0))/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "res 0\n");
    }

    #[test]
    fn compile_short_circuit_or() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"res {}\", 1 == 1 || (1 / 0))/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "res 1\n");
    }

    #[test]
    fn compile_function_call_simple() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new add(a, b)/\n    return a + b/\nfn/\nfn new main/\n    println(\"{}\", add(2, 3))/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(
            &exe,
            std::path::Path::new("/tmp/shiden_test_function_call_exe"),
        );
        println!("exe path: {}", exe.display());
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(String::from_utf8_lossy(&out.stdout), "5\n");
    }

    #[test]
    fn compile_function_call_debug() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(pd.join("src/main.sd"), "fn new add(a, b)/\n    println(\"in add\")/\n    return a + b/\nfn/\nfn new main/\n    println(\"before\")/\n    let _ = add(2, 3)/i64\n    println(\"after\")/\n    println(\"exit_main\")/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(
            &exe,
            std::path::Path::new("/tmp/shiden_test_function_call_debug_exe"),
        );
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "before\nin add\nafter\nexit_main\n"
        );
    }

    #[test]
    fn compile_function_call_many_args() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");

        fs::write(pd.join("src/main.sd"), "fn new add(a, b, c, d, e, f, g, h)/\n    println(\"args {} {} {} {} {} {} {} {}\", a, b, c, d, e, f, g, h)/\n    return a + b + c + d + e + f + g + h/\nfn/\nfn new main/\n    println(\"{}\", add(1,2,3,4,5,6,7,8))/\nfn/").expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_many_args_exe"));
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "args 1 2 3 4 5 6 7 8\n36\n"
        );
    }

    #[test]
    fn debug_array_run() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let mut a = []/array\n    push(a, 'H')/\n    push(a, 'i')/\n    println(\"{} {}\", a[0], a[1])/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = "test"\n[build]\ntargets = ["x86_64-linux"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        println!("exe path: {}", exe.display());

        let _ = std::fs::copy(&exe, std::path::Path::new("/tmp/shiden_debug_exe"));
        let objdump = Command::new("objdump").arg("-d").arg(&exe).output().ok();
        if let Some(od) = objdump {
            println!("objdump stdout:\n{}", String::from_utf8_lossy(&od.stdout));
            println!("objdump stderr:\n{}", String::from_utf8_lossy(&od.stderr));
        }

        let out = run_exe(&exe).expect("run exe");

        println!(
            "status: {:?}\nstdout: {}\nstderr: {}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );

        assert!(out.status.success());
        assert_eq!(String::from_utf8_lossy(&out.stdout), "H i\n");
    }

    #[test]
    fn compile_assign_index() {
        let td = tempdir().expect("tempdir");
        let pd = td.path();
        fs::create_dir_all(pd.join("src")).expect("mkdir");
        fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    let mut a = []/array\n    push(a, 1)/\n    a[0] = 42/\n    println(\"{}\", a[0])/\nfn/",
        )
        .expect("write src");
        fs::write(
            pd.join("shiden.toml"),
            r#"[project]\nname = \"test\"\n[build]\ntargets = [\"x86_64-linux\"]"#,
        )
        .expect("write mf");

        let exe = compile_project(pd, "test", "x86_64-linux", None).expect("compile");
        let out = run_exe(&exe).expect("run exe");
        if !out.status.success() {
            panic!(
                "exe failed: {} stderr:{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert_eq!(String::from_utf8_lossy(&out.stdout), "42\n");
    }
}
