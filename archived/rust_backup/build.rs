use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn compile_asm(asm: &Path, bin: &Path, target: &str) {
    println!("cargo:rerun-if-changed={}", asm.display());

    if let Ok(status) = Command::new("nasm")
        .arg("-f")
        .arg("bin")
        .arg(asm)
        .arg("-o")
        .arg(bin)
        .status()
    {
        if status.success() {
            return;
        }
        eprintln!("nasm failed on {} (status {})", asm.display(), status);
    }

    if which::which("llvm-mc").is_ok() && which::which("llvm-objcopy").is_ok() {
        let triple = if target == "windows" {
            "x86_64-pc-windows-msvc"
        } else {
            "x86_64-unknown-linux-gnu"
        };
        let obj = bin.with_extension("obj");
        let status = Command::new("llvm-mc")
            .arg("-triple")
            .arg(triple)
            .arg("-filetype=obj")
            .arg(asm)
            .arg("-o")
            .arg(&obj)
            .status()
            .expect("failed to spawn llvm-mc");
        if !status.success() {
            panic!("llvm-mc failed on {}", asm.display());
        }
        let status2 = Command::new("llvm-objcopy")
            .arg("-O")
            .arg("binary")
            .arg("--only-section=.text")
            .arg(&obj)
            .arg(bin)
            .status()
            .expect("failed to spawn llvm-objcopy");
        if !status2.success() {
            panic!("llvm-objcopy failed on {}", obj.display());
        }
        return;
    }

    panic!(
        "no assembler found (nasm or llvm-mc) to build {}",
        asm.display()
    );
}

fn main() {
    if env::var("CARGO_FEATURE_AUTO_HELPERS").is_err() {
        return;
    }

    let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    println!(
        "cargo:rerun-if-changed={}",
        manifest.join("libraries").display()
    );

    struct Helper {
        name: String,
        category: String,
        target: String,
        file: PathBuf,
    }
    let mut helpers: Vec<Helper> = Vec::new();

    let libs_dir = manifest.join("libraries");
    if libs_dir.exists() {
        for cat_entry in fs::read_dir(&libs_dir).unwrap() {
            let cat_entry = cat_entry.unwrap();
            if !cat_entry.file_type().unwrap().is_dir() {
                continue;
            }
            let cat = cat_entry.file_name().into_string().unwrap();
            for tgt_entry in fs::read_dir(cat_entry.path()).unwrap() {
                let tgt_entry = tgt_entry.unwrap();
                if !tgt_entry.file_type().unwrap().is_dir() {
                    continue;
                }
                let tgt = tgt_entry.file_name().into_string().unwrap();
                for file_entry in fs::read_dir(tgt_entry.path()).unwrap() {
                    let file_entry = file_entry.unwrap();
                    let path = file_entry.path();
                    if path.extension().and_then(|e| e.to_str()) == Some("asm") {
                        let stem = path.file_stem().unwrap().to_str().unwrap();

                        let full_name = match cat.as_str() {
                            "fs" => format!("fs_{}", stem),
                            "networking" => {
                                if stem == "server_new" {
                                    "net_server_new".into()
                                } else if stem == "server_accept" {
                                    "net_server_accept".into()
                                } else if stem == "send_byte" {
                                    "net_send_byte".into()
                                } else if stem == "send_raw" {
                                    "net_send_raw".into()
                                } else {
                                    format!("net_{}", stem)
                                }
                            }
                            "runtime" => stem.to_string(),
                            _other => stem.to_string(),
                        };
                        let bin_path = path.with_extension("bin");
                        compile_asm(&path, &bin_path, &tgt);
                        helpers.push(Helper {
                            name: full_name,
                            category: cat.clone(),
                            target: tgt.clone(),
                            file: bin_path,
                        });
                    }
                }
            }
        }
    }

    let mut src = String::new();

    src.push_str(
        "pub fn generated_get_helpers(target: &str) -> BTreeMap<&'static str, Vec<u8>> {\n",
    );
    src.push_str("    if target.contains(\"windows\") {\n");
    src.push_str("        let mut m = BTreeMap::new();\n");
    for h in &helpers {
        if h.target == "windows" {
            let rel = h.file.strip_prefix(&manifest).unwrap();
            src.push_str(&format!(
                "        m.insert(\"{}\", include_bytes!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/{}\")).to_vec());\n",
                h.name,
                rel.display()
            ));
        }
    }
    src.push_str("        for (name, bytes) in crate::libraries::networking::helpers::get_http_helpers(target) {\n");
    src.push_str("            m.insert(name, bytes);\n");
    src.push_str("        }\n");
    src.push_str("        return m;\n");
    src.push_str("    }\n\n");
    src.push_str("    let mut m = BTreeMap::new();\n");
    for h in &helpers {
        if h.target == "linux" {
            let rel = h.file.strip_prefix(&manifest).unwrap();
            src.push_str(&format!(
                "        m.insert(\"{}\", include_bytes!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/{}\")).to_vec());\n",
                h.name,
                rel.display()
            ));
        }
    }
    src.push_str("\n    for (name, bytes) in crate::libraries::networking::helpers::get_http_helpers(target) {\n");
    src.push_str("        m.insert(name, bytes);\n");
    src.push_str("    }\n");
    src.push_str("    m\n");
    src.push_str("}\n");

    let dest = out_dir.join("helpers.rs");
    fs::write(&dest, src).expect("failed to write helper source");

    let mut doc = String::new();
    doc.push_str("# Auto‑generated helper binaries\n\n");
    doc.push_str("The following files are produced by `build.rs` when the `auto-helpers` feature is enabled:\n\n");
    for h in &helpers {
        doc.push_str(&format!(
            "- `{}` ({}/{}) → `{}`\n",
            h.name,
            h.category,
            h.target,
            h.file.display()
        ));
    }
    fs::write(manifest.join("docs/helpers.md"), doc).expect("failed to write docs/helpers.md");
}
