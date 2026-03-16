use std::env;
use std::fs;
use std::process::exit;

fn print_usage() {
    eprintln!("shiden low <command> [options] <file>.sd");
    eprintln!("");
    eprintln!("Commands:");
    eprintln!("  parse <file>.sd           Print parsed AST/IR");
    eprintln!("  build --target <triple> <file>.sd   Emit object/exe for target");
    eprintln!("  run --target <triple> <file>.sd     Build and run locally (if host)");
}

mod backend_linux_x86;
mod bootstrap;
mod executor;
mod ini;
mod ir;
mod ir_types;
mod lexer;
mod parser;
mod target;
mod targets;

fn cmd_parse(path: &str) {
    let entries = parser::parse_file(path);
    let out = format!("{}.ir.json", path);
    match ir::write_ir(&out, &entries) {
        Ok(()) => println!("Wrote IR to {}", out),
        Err(e) => eprintln!("failed to write IR {}: {}", out, e),
    }
}

fn cmd_build(target: &str, path: &str) {
    let entries = parser::parse_file(path);
    let out_ir = format!("{}.ir.json", path);
    match ir::write_ir(&out_ir, &entries) {
        Ok(()) => println!("Wrote IR to {}", out_ir),
        Err(e) => {
            eprintln!("failed to write IR {}: {}", out_ir, e);
            return;
        }
    }

    let mut tcfg_opt: Option<target::Target> = None;
    if let Some(ini_text) = targets::get_ini(target) {
        let tcfg = target::parse_target_from_ini_text(ini_text);
        println!("Using embedded target: {} ({})", tcfg.name, tcfg.triple);
        println!(
            " - word_bits={}, pointer_bits={}, endianness={}",
            tcfg.word_bits, tcfg.pointer_bits, tcfg.endianness
        );
        println!(
            " - return_register={:?}, arg_registers={:?}",
            tcfg.return_register, tcfg.arg_registers
        );
        let out_ini = format!("{}.target.ini", path);
        if let Err(e) = fs::write(&out_ini, ini_text) {
            eprintln!("failed to write target ini {}: {}", out_ini, e);
        } else {
            println!("Wrote target INI to {}", out_ini);
        }
        tcfg_opt = Some(tcfg);
    } else {
        println!(
            "No embedded INI for target '{}'; searching filesystem.",
            target
        );
    }

    let backend_exec = format!("compiler/backends/{}/backend", target);
    let backend_sd = format!("compiler/backends/{}/backend.sd", target);

    if target == "linux-x86" || target == "x86_64-pc-linux-gnu" {
        let out_bin = format!("{}.out", path);
        if let Some(ref tcfg) = tcfg_opt {
            match backend_linux_x86::emit_backend(tcfg, &out_ir, &out_bin) {
                Ok(()) => println!("Backend emitted {}", out_bin),
                Err(e) => eprintln!("backend error: {}", e),
            }
        } else {
            eprintln!("no target configuration available for embedded backend");
        }
    } else if std::path::Path::new(&backend_exec).exists() {
        println!("Found native backend binary: {}", backend_exec);
        let out_bin = format!("{}.out", path);
        let status = std::process::Command::new(&backend_exec)
            .arg("emit")
            .arg("--protocol")
            .arg("1")
            .arg("--target")
            .arg(target)
            .arg("--in")
            .arg(&out_ir)
            .arg("--out")
            .arg(&out_bin)
            .status();
        match status {
            Ok(s) if s.success() => println!("Backend emitted {}", out_bin),
            Ok(s) => eprintln!("backend failed with status: {}", s),
            Err(e) => eprintln!("failed to run backend: {}", e),
        }
    } else if std::path::Path::new(&backend_sd).exists() {
        println!("Found LowShiden backend program: {}", backend_sd);
        let out_bin = format!("{}.out", path);
        match bootstrap::run_backend_sd(tcfg_opt.as_ref(), &backend_sd, &out_ir, &out_bin) {
            Ok(()) => println!("Bootstrap backend emitted {}", out_bin),
            Err(e) => eprintln!("bootstrap backend error: {}", e),
        }
    } else {
        println!("No backend found for target '{}'.", target);
        println!(
            "Place a native backend at '{}' or a LowShiden backend at '{}'.",
            backend_exec, backend_sd
        );
        println!("IR file: {}", out_ir);
    }
}

fn cmd_run(target: &str, path: &str) {
    let mut candidates = Vec::new();
    candidates.push(path.to_string());
    candidates.push(format!("{}/src/main.sd", path.trim_end_matches('/')));
    candidates.push(format!("{}/main.sd", path.trim_end_matches('/')));
    candidates.push(format!("compiler/{}", path.trim_start_matches("./")));
    candidates.push(format!(
        "compiler/{}/src/main.sd",
        path.trim_start_matches("./").trim_end_matches('/')
    ));
    candidates.push(format!(
        "compiler/{}/main.sd",
        path.trim_start_matches("./").trim_end_matches('/')
    ));

    let mut resolved: Option<String> = None;
    for c in candidates {
        if std::path::Path::new(&c).is_file() {
            resolved = Some(c);
            break;
        }
    }

    let resolved_path = match resolved {
        Some(p) => p,
        None => {
            eprintln!("could not locate file for path '{}'", path);
            return;
        }
    };

    let entries = parser::parse_file(&resolved_path);
    if target == "host" || target == "linux-x86" {
        match executor::execute(&entries) {
            Ok(()) => println!("Program executed."),
            Err(e) => eprintln!("Execution error: {}", e),
        }
    } else {
        println!("Run not implemented for target '{}'.", target);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        exit(1);
    }

    let first = args.get(1).map(|s| s.as_str()).unwrap_or("");
    if first != "low" {
        print_usage();
        exit(1);
    }

    if args.len() < 3 {
        print_usage();
        exit(1);
    }

    let cmd = args.get(2).map(|s| s.as_str()).unwrap_or("");
    match cmd {
        "parse" => {
            if let Some(path) = args.get(3) {
                cmd_parse(path);
            } else {
                eprintln!("parse requires a file path");
                exit(1);
            }
        }
        "build" => {
            let mut target: Option<String> = None;
            let mut file: Option<String> = None;
            let mut i = 3;
            while i < args.len() {
                match args[i].as_str() {
                    "--target" => {
                        if i + 1 < args.len() {
                            target = Some(args[i + 1].clone());
                            i += 2;
                        } else {
                            eprintln!("--target requires a value");
                            exit(1);
                        }
                    }
                    s => {
                        file = Some(s.to_string());
                        i += 1;
                    }
                }
            }
            let file = match file {
                Some(f) => f,
                None => {
                    eprintln!("build requires a file path");
                    exit(1);
                }
            };
            let target = target.unwrap_or_else(|| "host".to_string());
            cmd_build(&target, &file);
        }
        "run" => {
            let mut target: Option<String> = None;
            let mut file: Option<String> = None;
            let mut i = 3;
            while i < args.len() {
                match args[i].as_str() {
                    "--target" => {
                        if i + 1 < args.len() {
                            target = Some(args[i + 1].clone());
                            i += 2;
                        } else {
                            eprintln!("--target requires a value");
                            exit(1);
                        }
                    }
                    s => {
                        file = Some(s.to_string());
                        i += 1;
                    }
                }
            }
            let file = match file {
                Some(f) => f,
                None => {
                    eprintln!("run requires a file path");
                    exit(1);
                }
            };
            let target = target.unwrap_or_else(|| "host".to_string());
            cmd_run(&target, &file);
        }
        _ => {
            print_usage();
            exit(1);
        }
    }
}
