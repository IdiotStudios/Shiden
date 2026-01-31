use clap::{CommandFactory, Parser, Subcommand};
use std::fs;
use std::io::{self, Read};

#[derive(Parser)]
#[command(author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Parse { file: Option<String> },

    Run { file: Option<String> },

    Check { file: Option<String> },

    New { name: String },
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
        Some(Commands::Run { file }) => match read_source(file) {
            Ok(src) => match crate::frontend::parse(&src) {
                Ok(prog) => {
                    let mut it = crate::interpreter::Interpreter::new();
                    match it.run_program(&prog) {
                        Ok(()) => print!("{}", it.output()),
                        Err(e) => {
                            eprintln!("Runtime error: {}", e);
                            std::process::exit(1);
                        }
                    }
                }
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
        Some(Commands::Check { file }) => match read_source(file) {
            Ok(src) => match crate::frontend::parse(&src) {
                Ok(prog) => {
                    let has_main = prog.items.iter().any(|it| matches!(it, crate::syntax::Item::Function { name, .. } if name == "main"));
                    if has_main {
                        println!("OK: parse + minimal checks passed");
                    } else {
                        eprintln!("Check failed: missing 'main' function");
                        std::process::exit(2);
                    }
                }
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
        Some(Commands::New { name }) => {
            match create_project_in(std::env::current_dir().expect("cwd"), name) {
                Ok(()) => println!("Created project '{}'", name),
                Err(e) => {
                    eprintln!("Failed to create project: {}", e);
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
