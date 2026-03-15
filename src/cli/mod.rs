use clap::{CommandFactory, Parser, Subcommand};
use serde::Deserialize;
use std::fs;
use std::io::{self, Read};

#[cfg(target_os = "windows")]
mod hires_timer {
    unsafe extern "system" {
        fn QueryPerformanceFrequency(lpFrequency: *mut i64) -> i32;
        fn QueryPerformanceCounter(lpPerformanceCount: *mut i64) -> i32;
    }

    pub struct HighResTimer {
        freq: u64,
        start: i64,
    }

    impl HighResTimer {
        pub fn now() -> Self {
            let mut freq: i64 = 0;
            let mut count: i64 = 0;
            unsafe {
                QueryPerformanceFrequency(&mut freq);
                QueryPerformanceCounter(&mut count);
            }
            HighResTimer {
                freq: freq as u64,
                start: count,
            }
        }

        pub fn elapsed(&self) -> HighResDuration {
            let mut count: i64 = 0;
            unsafe {
                QueryPerformanceCounter(&mut count);
            }
            let elapsed_counts = (count - self.start) as u64;
            let nanos = (elapsed_counts * 1_000_000_000) / self.freq;
            let picos = ((elapsed_counts * 1_000_000_000_000) / self.freq) % 1_000;
            HighResDuration { nanos, picos }
        }
    }

    pub struct HighResDuration {
        pub nanos: u64,
        pub picos: u64,
    }
}

#[cfg(target_os = "macos")]
mod hires_timer {
    unsafe extern "system" {
        fn mach_absolute_time() -> u64;
        fn mach_timebase_info(info: *mut MachTimebaseInfo) -> i32;
    }

    #[repr(C)]
    struct MachTimebaseInfo {
        numer: u32,
        denom: u32,
    }

    pub struct HighResTimer {
        start: u64,
        numer: u32,
        denom: u32,
    }

    impl HighResTimer {
        pub fn now() -> Self {
            let mut info = MachTimebaseInfo { numer: 0, denom: 0 };
            unsafe {
                mach_timebase_info(&mut info);
            }
            HighResTimer {
                start: unsafe { mach_absolute_time() },
                numer: info.numer,
                denom: info.denom,
            }
        }

        pub fn elapsed(&self) -> HighResDuration {
            let end = unsafe { mach_absolute_time() };
            let elapsed_ticks = end - self.start;
            let nanos = (elapsed_ticks * self.numer as u64) / self.denom as u64;
            let picos = ((elapsed_ticks * self.numer as u64 * 1_000) / self.denom as u64) % 1_000;
            HighResDuration { nanos, picos }
        }
    }

    pub struct HighResDuration {
        pub nanos: u64,
        pub picos: u64,
    }
}

#[cfg(target_os = "linux")]
mod hires_timer {

    unsafe extern "C" {
        fn clock_gettime(clk_id: i32, tp: *mut Timespec) -> i32;
    }

    const CLOCK_MONOTONIC_RAW: i32 = 4;

    #[repr(C)]
    struct Timespec {
        tv_sec: isize,
        tv_nsec: isize,
    }

    pub struct HighResTimer {
        start_sec: isize,
        start_nsec: isize,
    }

    impl HighResTimer {
        pub fn now() -> Self {
            let mut ts = Timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            unsafe {
                clock_gettime(CLOCK_MONOTONIC_RAW, &mut ts);
            }
            HighResTimer {
                start_sec: ts.tv_sec,
                start_nsec: ts.tv_nsec,
            }
        }

        pub fn elapsed(&self) -> HighResDuration {
            let mut ts = Timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            unsafe {
                clock_gettime(CLOCK_MONOTONIC_RAW, &mut ts);
            }
            let mut sec = ts.tv_sec - self.start_sec;
            let mut nsec = ts.tv_nsec - self.start_nsec;
            if nsec < 0 {
                sec -= 1;
                nsec += 1_000_000_000;
            }
            let nanos = if sec < 0 {
                0
            } else {
                (sec as u64)
                    .saturating_mul(1_000_000_000)
                    .saturating_add(nsec as u64)
            };
            let picos = 0u64;
            HighResDuration { nanos, picos }
        }
    }

    pub struct HighResDuration {
        pub nanos: u64,
        pub picos: u64,
    }
}

fn red(s: &str) -> String {
    format!("\x1b[31m{}\x1b[0m", s)
}

fn green(s: &str) -> String {
    format!("\x1b[32m{}\x1b[0m", s)
}

fn cyan(s: &str) -> String {
    format!("\x1b[36m{}\x1b[0m", s)
}

fn get_current_platform_target() -> &'static str {
    if cfg!(target_os = "windows") {
        "x86_64-windows"
    } else if cfg!(target_os = "macos") {
        "x86_64-macos"
    } else {
        "x86_64-linux"
    }
}

fn select_target(targets: Vec<String>) -> String {
    if targets.is_empty() {
        return get_current_platform_target().to_string();
    }

    let current_platform = get_current_platform_target();

    if let Some(target) = targets.iter().find(|t| {
        if cfg!(target_os = "windows") {
            t.contains("windows")
        } else if cfg!(target_os = "macos") {
            t.contains("macos")
        } else {
            t.contains("linux")
        }
    }) {
        return target.clone();
    }

    targets
        .first()
        .cloned()
        .unwrap_or_else(|| current_platform.to_string())
}

fn format_hires_duration(duration: hires_timer::HighResDuration) -> String {
    let total_nanos = duration.nanos;
    let total_secs = total_nanos / 1_000_000_000;
    let minutes = total_secs / 60;
    let secs = total_secs % 60;

    let subsec_nanos = total_nanos % 1_000_000_000;
    let millis = subsec_nanos / 1_000_000;
    let nanos = subsec_nanos % 1_000;
    let picos = duration.picos;

    let mut parts = Vec::new();
    if minutes > 0 {
        parts.push(format!("{}m", minutes));
    }
    if secs > 0 {
        parts.push(format!("{}s", secs));
    }
    if millis > 0 {
        parts.push(format!("{}ms", millis));
    }
    if nanos > 0 {
        parts.push(format!("{}ns", nanos));
    }
    if picos > 0 {
        parts.push(format!("{}ps", picos));
    }

    if parts.is_empty() {
        "0ns".to_string()
    } else {
        parts.join(" ")
    }
}

fn split_error_loc(err: &str) -> (String, Option<(usize, usize)>) {
    if let Some(idx) = err.rfind(" (line ") {
        let msg = err[..idx].to_string();
        let tail = &err[idx + 2..];
        let tail = tail.trim_end_matches(')');
        let mut parts = tail.split(',');
        let line_part = parts.next().unwrap_or("");
        let col_part = parts.next().unwrap_or("");
        let line = line_part.trim_start_matches("line ").trim();
        let col = col_part.trim_start_matches("col ").trim();
        if let (Ok(l), Ok(c)) = (line.parse::<usize>(), col.parse::<usize>()) {
            return (msg, Some((l, c)));
        }
    }
    (err.to_string(), None)
}

fn format_parse_error(file_label: &str, err: &str) -> String {
    let (msg, loc) = split_error_loc(err);
    if let Some((line, col)) = loc {
        format!("{}:{}:{}: {}", file_label, line, col, msg)
    } else {
        format!("{}: {}", file_label, err)
    }
}

#[derive(Parser)]
#[command(author, version, about)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    #[command(about = "Parse and analyze a Shiden source file")]
    Parse { file: Option<String> },

    #[command(about = "Execute Shiden source code")]
    Run {
        file: Option<String>,
        #[arg(long)]
        out: Option<String>,
    },

    #[command(about = "Validate syntax and check for compilation errors")]
    Check {
        file: Option<String>,
        #[arg(long)]
        format: Option<String>,
    },

    #[command(about = "Create a new Shiden project")]
    New { name: String },

    #[command(about = "Compile a complete Shiden project")]
    Compile { manifest: Option<String> },

    #[command(about = "Check for and install updates")]
    Update {
        #[arg(long, help = "Check for available updates without installing")]
        check: bool,
        #[arg(long, help = "Compile and install from source")]
        from_source: bool,
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

fn has_manifest_in(dir: &std::path::Path) -> bool {
    dir.join("shiden.toml").exists()
}

fn default_run_file_for(file: &Option<String>, cwd: &std::path::Path) -> Option<String> {
    if file.is_some() {
        return file.clone();
    }
    if has_manifest_in(cwd) {
        Some(".".to_string())
    } else {
        None
    }
}

fn default_check_file_for(file: &Option<String>, cwd: &std::path::Path) -> Option<String> {
    if file.is_some() {
        return file.clone();
    }
    if has_manifest_in(cwd) {
        Some("src/main.sd".to_string())
    } else {
        None
    }
}

pub fn run() {
    let cli = Cli::parse();

    crate::update::check_for_updates_daily();

    match &cli.command {
        Some(Commands::Parse { file }) => match read_source(file) {
            Ok(src) => match crate::frontend::parse(&src) {
                Ok(prog) => println!("Parsed program: {:?}", prog),
                Err(e) => {
                    let label = file.clone().unwrap_or_else(|| "<stdin>".into());
                    eprintln!("{}: {}", red("Parse error"), format_parse_error(&label, &e));
                    std::process::exit(2);
                }
            },
            Err(e) => {
                eprintln!("I/O error reading source: {}", e);
                std::process::exit(2);
            }
        },
        Some(Commands::Run { file, out }) => {
            let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
            let run_file = default_run_file_for(file, &cwd);
            match run_file {
            Some(p) => {
                let path = std::path::Path::new(&p);
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
                            let target = select_target(targets);

                            let compile_start = hires_timer::HighResTimer::now();
                            match crate::build::compile_project(
                                path,
                                &proj_name,
                                &target,
                                mf.build.as_ref().and_then(|b| b.opt_level),
                            ) {
                                Ok(exe) => {
                                    let compile_time = compile_start.elapsed();
                                    eprintln!(
                                        "{} {}",
                                        green("Compiled in"),
                                        cyan(&format_hires_duration(compile_time))
                                    );

                                    let run_start = hires_timer::HighResTimer::now();
                                    let mut cmd = std::process::Command::new(&exe);
                                    if let Some(out_path) = out.as_ref() {
                                        let file =
                                            std::fs::File::create(out_path).unwrap_or_else(|e| {
                                                eprintln!(
                                                    "{}: {}",
                                                    red("Failed to open output file"),
                                                    e
                                                );
                                                std::process::exit(1)
                                            });
                                        cmd.stdout(file);
                                    }
                                    let mut child = cmd.spawn().unwrap_or_else(|e| {
                                        eprintln!("failed to run: {}", e);
                                        std::process::exit(1)
                                    });
                                    let status = child.wait();
                                    let run_time = run_start.elapsed();

                                    if status.is_ok() {
                                        eprintln!(
                                            "{} {}",
                                            green("Finished in"),
                                            cyan(&format_hires_duration(run_time))
                                        );
                                    }
                                }
                                Err(e) => {
                                    eprintln!("{}: {}", red("Build failed"), e);
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
                            Ok(out_text) => {
                                if let Some(out_path) = out.as_ref() {
                                    std::fs::write(out_path, out_text).unwrap_or_else(|e| {
                                        eprintln!("{}: {}", red("Failed to write output file"), e);
                                        std::process::exit(1)
                                    });
                                } else {
                                    print!("{}", out_text)
                                }
                            }
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
                            Ok(out_text) => {
                                if let Some(out_path) = out.as_ref() {
                                    std::fs::write(out_path, out_text).unwrap_or_else(|e| {
                                        eprintln!("{}: {}", red("Failed to write output file"), e);
                                        std::process::exit(1)
                                    });
                                } else {
                                    print!("{}", out_text)
                                }
                            }
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
                    Ok(out_text) => {
                        if let Some(out_path) = out.as_ref() {
                            std::fs::write(out_path, out_text).unwrap_or_else(|e| {
                                eprintln!("{}: {}", red("Failed to write output file"), e);
                                std::process::exit(1)
                            });
                        } else {
                            print!("{}", out_text)
                        }
                    }
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
            }
        }

        Some(Commands::Check { file, format }) => {
            let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
            let check_file = default_check_file_for(file, &cwd);
            match read_source(&check_file) {
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
                                let file_path = check_file.clone().unwrap_or_else(|| "".into());
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
                            let file_path = check_file.clone().unwrap_or_else(|| "".into());
                            let (msg, loc) = split_error_loc(&e);
                            let (line, col) = loc.unwrap_or((1, 1));
                            println!(
                                "{{\"diagnostics\":[{{\"file\":\"{}\",\"line\":{},\"col\":{},\"severity\":\"error\",\"message\":\"{}\"}}]}}",
                                file_path,
                                line,
                                col,
                                escape_json(&msg)
                            );
                        } else {
                            let label = check_file.clone().unwrap_or_else(|| "<stdin>".into());
                            eprintln!("Parse error: {}", format_parse_error(&label, &e));
                        }
                        std::process::exit(2);
                    }
                }
            }
            Err(e) => {
                if matches!(format.as_deref(), Some("json")) {
                    let file_path = check_file.clone().unwrap_or_else(|| "".into());
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
            }
        }

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
                    let mut targets = mf
                        .build
                        .as_ref()
                        .and_then(|b| b.targets.clone())
                        .unwrap_or_default();

                    if targets.is_empty() {
                        targets.push(get_current_platform_target().to_string());
                    }

                    for t in targets {
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
                                eprintln!("Build failed for {}: {}", t, e);
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

        Some(Commands::Update { check, from_source }) => {
            if *check {
                match crate::update::check_for_update() {
                    Ok(Some(version)) => {
                        println!(
                            "{} {} is available (you have {})",
                            green("✓"),
                            version,
                            env!("CARGO_PKG_VERSION")
                        );
                        println!("Run '{}' to update", cyan("shiden update"));
                    }
                    Ok(None) => {
                        println!(
                            "{} You are running the latest version ({})",
                            green("✓"),
                            env!("CARGO_PKG_VERSION")
                        );
                    }
                    Err(e) => {
                        eprintln!("{}: {}", red("Failed to check for updates"), e);
                        std::process::exit(1);
                    }
                }
            } else if *from_source {
                match crate::update::update_from_source() {
                    Ok(_) => {
                        println!(
                            "{} Successfully compiled and installed from source",
                            green("✓")
                        );
                        println!(
                            "{}",
                            cyan("Please restart shiden for the changes to take effect")
                        );
                    }
                    Err(e) => {
                        eprintln!("{}: {}", red("Update from source failed"), e);
                        std::process::exit(1);
                    }
                }
            } else {
                match crate::update::perform_update() {
                    Ok(updated) => {
                        if updated {
                            println!("{} Successfully updated to the latest version", green("✓"));
                            println!(
                                "{}",
                                cyan("Please restart shiden for the changes to take effect")
                            );
                        } else {
                            println!(
                                "{} You are running the latest version ({})",
                                green("✓"),
                                env!("CARGO_PKG_VERSION")
                            );
                        }
                    }
                    Err(e) => {
                        eprintln!("{}: {}", red("Update failed"), e);
                        std::process::exit(1);
                    }
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

    let main_sd = "fn new main/\n    println(\"Hello world\")/\nfn/";
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
    let mut cmd = std::process::Command::new(&exe);
    let out = cmd.output().map_err(|e| e.to_string())?;
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
#[allow(dead_code)]
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
            Commands::Run { file, .. } => assert!(file.is_none()),
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
    fn default_run_file_uses_cwd_project() {
        let td = tempfile::tempdir().expect("tempdir");
        std::fs::write(td.path().join("shiden.toml"), "[project]\nname=\"x\"").expect("write");
        let got = default_run_file_for(&None, td.path());
        assert_eq!(got, Some(".".to_string()));
    }

    #[test]
    fn default_check_file_uses_cwd_project_entry() {
        let td = tempfile::tempdir().expect("tempdir");
        std::fs::write(td.path().join("shiden.toml"), "[project]\nname=\"x\"").expect("write");
        let got = default_check_file_for(&None, td.path());
        assert_eq!(got, Some("src/main.sd".to_string()));
    }

    #[test]
    fn manifest_parsing_reads_targets() {
        let s = r#"[project]
name = "test"
[build]
targets = ["x86_64-linux", "x86_64-windows"]"#;
        let m: Manifest = toml::from_str(s).expect("parse manifest");
        let targets = m.build.unwrap().targets.unwrap();
        assert_eq!(
            targets,
            vec!["x86_64-linux".to_string(), "x86_64-windows".to_string()]
        );
    }

    #[test]
    fn compile_command_builds_all_manifest_targets() {
        use tempfile::tempdir;
        let td = tempdir().expect("tempdir");
        let pd = td.path();

        std::fs::create_dir_all(pd.join("src")).expect("create src");
        std::fs::write(
            pd.join("src/main.sd"),
            "fn new main/\n    println(\"hi\")/\nfn/",
        )
        .expect("write main");

        std::fs::write(
            pd.join("shiden.toml"),
            r#"[project]
name = "multi"
[build]
targets = ["x86_64-linux", "x86_64-windows"]"#,
        )
        .expect("write manifest");

        let mf = load_manifest_from_path(&pd.join("shiden.toml")).expect("load manifest");
        let proj_name = mf
            .project
            .and_then(|p| p.name)
            .unwrap_or_else(|| "multi".into());
        let targets = mf.build.unwrap().targets.unwrap();

        for t in targets {
            let exe = crate::build::compile_project(pd, &proj_name, &t, None)
                .unwrap_or_else(|_| panic!("compile target {}", t));
            assert!(
                exe.exists(),
                "executable for {} not found: {}",
                t,
                exe.display()
            );
        }
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
