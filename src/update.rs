use semver::Version;

const GITHUB_OWNER: &str = "IdiotStudios";
const GITHUB_REPO: &str = "Shiden";

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
