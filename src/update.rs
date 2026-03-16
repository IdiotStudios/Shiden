use semver::Version;
use std::fs;
use std::path::PathBuf;

const GITHUB_OWNER: &str = "IdiotStudios";
const GITHUB_REPO: &str = "Shiden";

fn get_shiden_config_dir() -> PathBuf {
    #[cfg(target_os = "windows")]
    {
        if let Ok(appdata) = std::env::var("APPDATA") {
            return PathBuf::from(appdata).join("shiden");
        }
        if let Ok(userprofile) = std::env::var("USERPROFILE") {
            return PathBuf::from(userprofile).join(".shiden");
        }
    }

    if let Ok(config_home) = std::env::var("XDG_CONFIG_HOME") {
        return PathBuf::from(config_home).join("shiden");
    }

    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home).join(".config").join("shiden");
    }

    PathBuf::from(".shiden")
}

fn get_last_update_check_file() -> PathBuf {
    get_shiden_config_dir().join(".last_update_check")
}

fn should_check_for_updates() -> bool {
    if std::env::var("SHIDEN_CHECK_UPDATES").is_ok_and(|v| v == "0" || v.to_lowercase() == "false")
    {
        return false;
    }

    let check_file = get_last_update_check_file();

    match fs::metadata(&check_file) {
        Ok(metadata) => {
            if let Ok(modified) = metadata.modified() {
                if let Ok(elapsed) = modified.elapsed() {
                    return elapsed.as_secs() > 86400;
                }
            }
            true
        }
        Err(_) => true,
    }
}

fn update_last_check_timestamp() {
    let check_file = get_last_update_check_file();
    if let Some(parent) = check_file.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(&check_file, "");
}

pub fn check_for_updates_daily() {
    if !should_check_for_updates() {
        return;
    }

    if let Ok(Some(new_version)) = check_for_update() {
        let current_version = env!("CARGO_PKG_VERSION");
        let yellow = "\x1b[33m";
        let cyan = "\x1b[36m";
        let reset = "\x1b[0m";
        eprintln!();
        eprintln!(
            "{}╭─ Update available ─────────────────────╮{}",
            yellow, reset
        );
        eprintln!(
            "{}│{} Shiden {} → {} {}{}",
            yellow, cyan, current_version, new_version, yellow, reset
        );
        eprintln!(
            "{}│{} Run {} to install {}",
            yellow, cyan, "shiden update", yellow
        );
        eprintln!(
            "{}╰────────────────────────────────────────╯{}",
            yellow, reset
        );
        eprintln!();
    }

    update_last_check_timestamp();
}

pub fn check_for_update() -> Result<Option<String>, Box<dyn std::error::Error>> {
    let current_version = Version::parse(env!("CARGO_PKG_VERSION"))?;
    let latest_version = fetch_latest_version()?;

    if latest_version > current_version {
        Ok(Some(latest_version.to_string()))
    } else {
        Ok(None)
    }
}

pub fn perform_update() -> Result<bool, Box<dyn std::error::Error>> {
    let current_version = Version::parse(env!("CARGO_PKG_VERSION"))?;
    let latest_version = fetch_latest_version()?;

    if latest_version <= current_version {
        return Ok(false);
    }

    let status = self_update::backends::github::Update::configure()
        .repo_owner(GITHUB_OWNER)
        .repo_name(GITHUB_REPO)
        .bin_name("shiden")
        .show_download_progress(true)
        .current_version(env!("CARGO_PKG_VERSION"))
        .build()?
        .update()?;

    eprintln!("Update status: `{}`", status.version());
    Ok(true)
}

fn fetch_latest_version() -> Result<Version, Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(5))
        .build()?;
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = client.get(&url).header("User-Agent", "Shiden-CLI").send()?;

    if !response.status().is_success() {
        return Err("Failed to fetch latest release info".into());
    }

    let release: GitHubRelease = response.json()?;
    let version_str = release.tag_name.trim_start_matches('v');
    Ok(Version::parse(version_str)?)
}

#[derive(serde::Deserialize, Debug)]
struct GitHubRelease {
    tag_name: String,
}

pub fn update_from_source() -> Result<(), Box<dyn std::error::Error>> {
    use std::process::Command;

    let temp_dir = std::env::temp_dir().join(format!("shiden-source-{}", std::process::id()));
    fs::create_dir_all(&temp_dir)?;

    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(30))
        .build()?;
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        GITHUB_OWNER, GITHUB_REPO
    );
    let response = client
        .get(&api_url)
        .header("User-Agent", "Shiden-CLI")
        .send()?;

    if !response.status().is_success() {
        let _ = fs::remove_dir_all(&temp_dir);
        return Err("Failed to fetch latest release info".into());
    }

    let release: GitHubRelease = response.json()?;
    let tag = release.tag_name;

    eprintln!("Downloading {} source...", tag);
    let download_url = format!(
        "https://github.com/{}/{}/archive/refs/tags/{}.tar.gz",
        GITHUB_OWNER, GITHUB_REPO, tag
    );

    let response = client.get(&download_url).send()?;
    if !response.status().is_success() {
        let _ = fs::remove_dir_all(&temp_dir);
        return Err("Failed to download source archive".into());
    }

    let bytes = response.bytes()?;
    let tar_path = temp_dir.join("source.tar.gz");
    fs::write(&tar_path, &bytes)?;

    eprintln!("Extracting source...");
    extract_tar_gz(&tar_path, &temp_dir)?;

    let extracted_dir = temp_dir.join(format!("{}-{}", GITHUB_REPO, tag.trim_start_matches('v')));
    if !extracted_dir.exists() {
        let _ = fs::remove_dir_all(&temp_dir);
        return Err("Failed to find extracted source directory".into());
    }

    eprintln!("Compiling Shiden from source...");
    let output = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .current_dir(&extracted_dir)
        .output()?;

    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        let _ = fs::remove_dir_all(&temp_dir);
        return Err("Failed to compile Shiden".into());
    }

    #[cfg(target_os = "windows")]
    let binary_name = "shiden.exe";
    #[cfg(not(target_os = "windows"))]
    let binary_name = "shiden";

    let binary_path = extracted_dir
        .join("target")
        .join("release")
        .join(binary_name);

    if !binary_path.exists() {
        let _ = fs::remove_dir_all(&temp_dir);
        return Err("Compiled binary not found".into());
    }

    let install_dir = if let Ok(install_dir) = std::env::var("INSTALL_DIR") {
        PathBuf::from(install_dir)
    } else {
        #[cfg(target_os = "windows")]
        let install_dir = {
            if let Ok(userprofile) = std::env::var("USERPROFILE") {
                PathBuf::from(userprofile).join(".shiden").join("bin")
            } else {
                PathBuf::from(".shiden").join("bin")
            }
        };
        #[cfg(not(target_os = "windows"))]
        let install_dir = {
            if let Ok(home) = std::env::var("HOME") {
                PathBuf::from(home).join(".local").join("bin")
            } else {
                PathBuf::from(".local").join("bin")
            }
        };
        install_dir
    };

    fs::create_dir_all(&install_dir)?;

    let target_path = install_dir.join(binary_name);
    fs::copy(&binary_path, &target_path)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = fs::Permissions::from_mode(0o755);
        fs::set_permissions(&target_path, perms)?;
    }

    let _ = fs::remove_dir_all(&temp_dir);

    Ok(())
}

fn extract_tar_gz(
    tar_path: &PathBuf,
    dest_dir: &PathBuf,
) -> Result<(), Box<dyn std::error::Error>> {
    let tar_file = fs::File::open(tar_path)?;
    let decoder = flate2::read::GzDecoder::new(tar_file);
    let mut archive = tar::Archive::new(decoder);
    archive.unpack(dest_dir)?;

    Ok(())
}
