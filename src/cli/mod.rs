use clap::{CommandFactory, Parser, Subcommand};
use serde::Deserialize;
use std::fs;
use std::io::{self, Read};
use std::thread;
use std::time::Duration;

#[derive(Parser)]
#[command(author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Parse {
        file: Option<String>,
    },

    Run {
        file: Option<String>,
    },

    Check {
        file: Option<String>,
        #[arg(long)]
        format: Option<String>,
    },

    New {
        name: String,
    },

    Compile {
        manifest: Option<String>,
    },
}

fn read_source(path: &Option<String>) -> io::Result<String> {
    match path {
        Some(p) => fs::read_to_string(p),
        None => {
            let mut s = String::new();
            io::stdin().read_to_string(&mut s)?;
            Ok(s)
        }
    }
}

pub fn run() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Parse { file }) => match read_source(file) {
            Ok(src) => match crate::frontend::parse(&src) {
                Ok(prog) => println!("Parsed program: {:?}", prog),
                Err(e) => {
                    eprintln!("Parse error: {}", e);
                    std::process::exit(2);
                }
            },
            Err(e) => {
                eprintln!("I/O error reading source: {}", e);
                std::process::exit(2);
            }
        },
        Some(Commands::Run { file }) => match file {
            Some(p) => {
                let path = std::path::Path::new(p);
                if path.is_dir() {
                    let manifest = path.join("shiden.toml");
                    if !manifest.exists() {
                        eprintln!("No shiden.toml found in directory: {}", p);
                        std::process::exit(2);
                    }
                    match load_manifest_from_path(&manifest) {
                        Ok(mf) => {
                            let proj_name = mf
                                .project
                                .and_then(|pr| pr.name)
                                .unwrap_or_else(|| "unnamed".into());
                            let targets = mf
                                .build
                                .as_ref()
                                .and_then(|b| b.targets.clone())
                                .unwrap_or_default();
                            let linux = targets.into_iter().find(|t| t.contains("linux"));
                            let target = linux.unwrap_or_else(|| "x86_64-linux".into());
                            match crate::build::compile_project(
                                &path,
                                &proj_name,
                                &target,
                                mf.build.as_ref().and_then(|b| b.opt_level),
                            ) {
                                Ok(exe) => {
                                    let out = std::process::Command::new(&exe)
                                        .output()
                                        .unwrap_or_else(|e| {
                                            eprintln!("failed to run: {}", e);
                                            std::process::exit(1)
                                        });
                                    use std::io::Write;
                                    std::io::stdout().write(&out.stdout).ok();
                                    std::io::stderr().write(&out.stderr).ok();
                                }
                                Err(e) => {
                                    eprintln!("Build failed: {}", e);
                                    std::process::exit(2);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("{}", e);
                            std::process::exit(2);
                        }
                    }
                } else if path.exists() {
                    match std::fs::read_to_string(path) {
                        Ok(src) => match compile_and_run_source(&src) {
                            Ok(out) => print!("{}", out),
                            Err(e) => {
                                eprintln!("Execution failed: {}", e);
                                std::process::exit(1);
                            }
                        },
                        Err(e) => {
                            eprintln!("I/O error reading source: {}", e);
                            std::process::exit(2);
                        }
                    }
                } else {
                    match read_source(&Some(p.clone())) {
                        Ok(src) => match compile_and_run_source(&src) {
                            Ok(out) => print!("{}", out),
                            Err(e) => {
                                eprintln!("Execution failed: {}", e);
                                std::process::exit(1);
                            }
                        },
                        Err(e) => {
                            eprintln!("I/O error reading source: {}", e);
                            std::process::exit(2);
                        }
                    }
                }
            }
            None => match read_source(&None) {
                Ok(src) => match compile_and_run_source(&src) {
                    Ok(out) => print!("{}", out),
                    Err(e) => {
                        eprintln!("Execution failed: {}", e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("I/O error reading source: {}", e);
                    std::process::exit(2);
                }
            },
        },

        Some(Commands::Check { file, format }) => match read_source(file) {
            Ok(src) => {
                let want_json = matches!(format.as_deref(), Some("json"));
                match crate::frontend::parse(&src) {
                    Ok(prog) => {
                        let has_main = prog.items.iter().any(|it| matches!(it, crate::syntax::Item::Function { name, .. } if name == "main"));
                        if has_main {
                            if want_json {
                                println!("{{\"diagnostics\":[]}}");
                            } else {
                                println!("OK: parse + minimal checks passed");
                            }
                        } else {
                            if want_json {
                                let file_path = file.clone().unwrap_or_else(|| "".into());
                                let msg = "missing 'main' function";
                                println!(
                                    "{{\"diagnostics\":[{{\"file\":\"{}\",\"line\":1,\"col\":1,\"severity\":\"error\",\"message\":\"{}\"}}]}}",
                                    file_path,
                                    escape_json(msg)
                                );
                            } else {
                                eprintln!("Check failed: missing 'main' function");
                            }
                            std::process::exit(2);
                        }
                    }
                    Err(e) => {
                        if want_json {
                            let file_path = file.clone().unwrap_or_else(|| "".into());
                            println!(
                                "{{\"diagnostics\":[{{\"file\":\"{}\",\"line\":1,\"col\":1,\"severity\":\"error\",\"message\":\"{}\"}}]}}",
                                file_path,
                                escape_json(&e)
                            );
                        } else {
                            eprintln!("Parse error: {}", e);
                        }
                        std::process::exit(2);
                    }
                }
            }
            Err(e) => {
                if matches!(format.as_deref(), Some("json")) {
                    let file_path = file.clone().unwrap_or_else(|| "".into());
                    println!(
                        "{{\"diagnostics\":[{{\"file\":\"{}\",\"line\":1,\"col\":1,\"severity\":\"error\",\"message\":\"{}\"}}]}}",
                        file_path,
                        escape_json(&format!("I/O error reading source: {}", e))
                    );
                } else {
                    eprintln!("I/O error reading source: {}", e);
                }
                std::process::exit(2);
            }
        },

        Some(Commands::New { name }) => {
            match create_project_in(std::env::current_dir().expect("cwd"), name) {
                Ok(()) => println!("Created project '{}'", name),
                Err(e) => {
                    eprintln!("Failed to create project: {}", e);
                    std::process::exit(2);
                }
            }
        }

        Some(Commands::Compile { manifest }) => {
            let manifest_path = match resolve_manifest_path(manifest.as_deref()) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(2);
                }
            };

            match load_manifest_from_path(&manifest_path) {
                Ok(mf) => {
                    let proj_dir = manifest_path
                        .parent()
                        .unwrap_or(std::path::Path::new("."))
                        .to_path_buf();
                    let proj_name = mf
                        .project
                        .and_then(|p| p.name)
                        .unwrap_or_else(|| "unnamed".to_string());
                    let targets = mf
                        .build
                        .as_ref()
                        .and_then(|b| b.targets.clone())
                        .unwrap_or_default();
                    let linux: Vec<_> = targets
                        .into_iter()
                        .filter(|t| t.contains("linux"))
                        .collect();
                    if linux.is_empty() {
                        eprintln!(
                            "No linux targets found in manifest; only linux is supported for now."
                        );
                        std::process::exit(2);
                    }

                    for t in linux {
                        eprintln!("Compiling for {} ...", t);

                        let outdir = proj_dir.join("build").join(&t);
                        if let Err(e) = std::fs::create_dir_all(&outdir) {
                            eprintln!("Failed to create output dir: {}", e);
                            std::process::exit(2);
                        }
                        match crate::build::compile_project(
                            &proj_dir,
                            &proj_name,
                            &t,
                            mf.build.as_ref().and_then(|b| b.opt_level),
                        ) {
                            Ok(p) => println!("Built {} for {} -> {}", proj_name, t, p.display()),
                            Err(e) => {
                                eprintln!("Build failed: {}", e);
                                std::process::exit(2);
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(2);
                }
            }
        }

        None => {
            Cli::command().print_help().ok();
            println!();
        }
    }
}

fn create_project_in(base: std::path::PathBuf, name: &str) -> Result<(), String> {
    let dir = base.join(name);
    if dir.exists() {
        return Err(format!("Path '{}' already exists", dir.display()));
    }
    std::fs::create_dir_all(dir.join("src")).map_err(|e| e.to_string())?;

    let main_sd = "fn new main/\n    println(\"Hello world\")/unit\nfn/";
    std::fs::write(dir.join("src/main.sd"), main_sd).map_err(|e| e.to_string())?;

    let manifest = format!(
        r#"# shiden.toml - project manifest
[project]
name = "{name}"
version = "0.1.0"
type = "binary"

[build]
opt_level = 3
targets = ["x86_64-linux", "x86_64-windows"]

[dependencies]
# "pkg" = "version"
"#
    );

    std::fs::write(dir.join("shiden.toml"), manifest).map_err(|e| e.to_string())?;
    Ok(())
}

fn compile_and_run_source(src: &str) -> Result<String, String> {
    use tempfile::tempdir;
    let td = tempdir().map_err(|e| e.to_string())?;
    let pd = td.path();
    std::fs::create_dir_all(pd.join("src")).map_err(|e| e.to_string())?;
    std::fs::write(pd.join("src/main.sd"), src).map_err(|e| e.to_string())?;
    std::fs::write(
        pd.join("shiden.toml"),
        r#"[project]
name = "tmp"
[build]
targets = ["x86_64-linux"]"#,
    )
    .map_err(|e| e.to_string())?;
    let exe = crate::build::compile_project(pd, "tmp", "x86_64-linux", None)?;
    let out = std::process::Command::new(&exe)
        .output()
        .map_err(|e| e.to_string())?;
    if out.status.success() {
        Ok(String::from_utf8_lossy(&out.stdout).to_string())
    } else {
        Err(format!(
            "Process failed: {} stderr:{}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        ))
    }
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

#[derive(Deserialize)]
struct Manifest {
    project: Option<Project>,
    build: Option<Build>,
}

#[derive(Deserialize)]
struct Project {
    name: Option<String>,
    version: Option<String>,
    #[serde(rename = "type")]
    proj_type: Option<String>,
}

#[derive(Deserialize, Clone)]
struct Build {
    opt_level: Option<i32>,
    targets: Option<Vec<String>>,
}

fn resolve_manifest_path(arg: Option<&str>) -> Result<std::path::PathBuf, String> {
    let path = match arg {
        Some(p) => std::path::PathBuf::from(p),
        None => std::env::current_dir().map_err(|e| e.to_string())?,
    };
    let manifest_path = if path.is_dir() {
        path.join("shiden.toml")
    } else {
        path
    };
    if !manifest_path.exists() {
        return Err(format!(
            "Manifest '{}' does not exist",
            manifest_path.display()
        ));
    }
    Ok(manifest_path)
}

fn load_manifest_from_path(manifest_path: &std::path::Path) -> Result<Manifest, String> {
    let s = std::fs::read_to_string(manifest_path).map_err(|e| {
        format!(
            "I/O error reading manifest '{}': {}",
            manifest_path.display(),
            e
        )
    })?;
    toml::from_str::<Manifest>(&s).map_err(|e| format!("Failed to parse manifest: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::env;

    #[test]
    fn clap_parses_run_stdin() {
        let args = vec!["shiden", "run"];
        let cli = Cli::parse_from(args);
        match cli.command.unwrap() {
            Commands::Run { file } => assert!(file.is_none()),
            _ => panic!("expected run subcommand"),
        }
    }

    #[test]
    fn clap_parses_parse_file() {
        let args = vec!["shiden", "parse", "main.sd"];
        let cli = Cli::parse_from(args);
        match cli.command.unwrap() {
            Commands::Parse { file } => assert_eq!(file.unwrap(), "main.sd".to_string()),
            _ => panic!("expected parse subcommand"),
        }
    }

    #[test]
    fn clap_parses_check_json_flag() {
        let args = vec!["shiden", "check", "main.sd", "--format", "json"];
        let cli = Cli::parse_from(args);
        match cli.command.unwrap() {
            Commands::Check { file, format } => {
                assert_eq!(file.unwrap(), "main.sd");
                assert_eq!(format.unwrap(), "json");
            }
            _ => panic!("expected check subcommand"),
        }
    }

    #[test]
    fn clap_parses_compile_no_arg() {
        let args = vec!["shiden", "compile"];
        let cli = Cli::parse_from(args);
        match cli.command.unwrap() {
            Commands::Compile { manifest } => assert!(manifest.is_none()),
            _ => panic!("expected compile subcommand"),
        }
    }

    #[test]
    fn manifest_parsing_filters_linux() {
        let s = r#"[project]
name = "test"
[build]
targets = ["x86_64-linux", "x86_64-windows"]"#;
        let m: Manifest = toml::from_str(s).expect("parse manifest");
        let targets = m.build.unwrap().targets.unwrap();
        let linux: Vec<_> = targets
            .into_iter()
            .filter(|t| t.contains("linux"))
            .collect();
        assert_eq!(linux, vec!["x86_64-linux".to_string()]);
    }

    #[test]
    fn clap_parses_compile_dir_arg() {
        let args = vec!["shiden", "compile", "test/"];
        let cli = Cli::parse_from(args);
        match cli.command.unwrap() {
            Commands::Compile { manifest } => assert_eq!(manifest.unwrap(), "test/"),
            _ => panic!("expected compile subcommand"),
        }
    }

    #[test]
    fn resolve_manifest_path_dir_and_file() {
        let td = tempfile::tempdir().expect("tempdir");
        let td_path = td.path();
        let manifest_path = td_path.join("shiden.toml");
        std::fs::write(
            &manifest_path,
            r#"[project]
name = "t"
[build]
targets = ["x86_64-linux"]"#,
        )
        .expect("write manifest");
        let resolved = resolve_manifest_path(Some(td_path.to_str().unwrap())).expect("resolve dir");
        assert_eq!(resolved, manifest_path);

        let mf2 = td_path.join("other.toml");
        std::fs::write(
            &mf2,
            "[project]
name = \"x\"",
        )
        .expect("write");
        let resolved2 = resolve_manifest_path(Some(mf2.to_str().unwrap())).expect("resolve file");
        assert_eq!(resolved2, mf2);
    }

    #[test]
    fn load_manifest_reads_and_parses() {
        let td = tempfile::tempdir().expect("tempdir");
        let manifest_path = td.path().join("shiden.toml");
        std::fs::write(
            &manifest_path,
            r#"[project]
name = "t"
[build]
targets = ["x86_64-linux"]"#,
        )
        .expect("write manifest");
        let m = load_manifest_from_path(&manifest_path).expect("load manifest");
        let ts = m.build.unwrap().targets.unwrap();
        assert_eq!(ts, vec!["x86_64-linux".to_string()]);
    }
    #[test]
    fn new_creates_manifest_toml() {
        let base = env::temp_dir().join(format!(
            "shiden_test_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let _ = std::fs::remove_dir_all(&base);
        let res = create_project_in(base.clone(), "testproj");
        assert!(res.is_ok());
        let mf = base.join("testproj").join("shiden.toml");
        assert!(mf.exists());
        let s = std::fs::read_to_string(mf).expect("read manifest");
        assert!(s.contains("[project]"));
        assert!(s.contains("[build]"));
        assert!(s.contains("opt_level"));

        let _ = std::fs::remove_dir_all(&base);
    }
}
