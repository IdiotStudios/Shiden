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

pub async fn check_for_updates_daily() {
    if !should_check_for_updates() {
        return;
    }

    match tokio::time::timeout(std::time::Duration::from_secs(5), check_for_update()).await {
        Ok(Ok(Some(new_version))) => {
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
        _ => {}
    }

    update_last_check_timestamp();
}

pub async fn check_for_update() -> Result<Option<String>, Box<dyn std::error::Error>> {
    let current_version = Version::parse(env!("CARGO_PKG_VERSION"))?;
    let latest_version = fetch_latest_version().await?;

    if latest_version > current_version {
        Ok(Some(latest_version.to_string()))
    } else {
        Ok(None)
    }
}

pub async fn perform_update() -> Result<bool, Box<dyn std::error::Error>> {
    let current_version = Version::parse(env!("CARGO_PKG_VERSION"))?;
    let latest_version = fetch_latest_version().await?;

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

async fn fetch_latest_version() -> Result<Version, Box<dyn std::error::Error>> {
    let client = reqwest::Client::new();
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases/latest",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = client
        .get(&url)
        .header("User-Agent", "Shiden-CLI")
        .send()
        .await?;

    if !response.status().is_success() {
        return Err("Failed to fetch latest release info".into());
    }

    let release: GitHubRelease = response.json().await?;
    let version_str = release.tag_name.trim_start_matches('v');
    Ok(Version::parse(version_str)?)
}

#[derive(serde::Deserialize, Debug)]
struct GitHubRelease {
    tag_name: String,
}
