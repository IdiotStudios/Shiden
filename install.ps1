# Shiden Installation Script for Windows
# Usage: powershell -ExecutionPolicy Bypass -File install.ps1

param(
    [string]$InstallDir = "$env:USERPROFILE\.shiden\bin"
)

$ErrorActionPreference = "Stop"

Write-Host "Detecting system..." -ForegroundColor Cyan

$GitHubOwner = "IdiotStudios"
$GitHubRepo = "Shiden"
$BinaryName = "shiden.exe"
$AssetName = "shiden-windows-x86_64.exe"

# Create install directory if it doesn't exist
if (!(Test-Path -Path $InstallDir)) {
    New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
}

Write-Host "Fetching latest release..." -ForegroundColor Cyan

# Get the latest release
try {
    $LatestRelease = Invoke-RestMethod -Uri "https://api.github.com/repos/$GitHubOwner/$GitHubRepo/releases/latest" -Headers @{"User-Agent" = "PowerShell"}
    $Tag = $LatestRelease.tag_name
} catch {
    Write-Host "Failed to fetch latest release: $_" -ForegroundColor Red
    exit 1
}

if ([string]::IsNullOrEmpty($Tag)) {
    Write-Host "Failed to determine latest version" -ForegroundColor Red
    exit 1
}

Write-Host "Latest version: $Tag" -ForegroundColor Green

$DownloadUrl = "https://github.com/$GitHubOwner/$GitHubRepo/releases/download/$Tag/$AssetName"
$TmpFile = Join-Path $env:TEMP "$AssetName.tmp"

Write-Host "Downloading binary..." -ForegroundColor Cyan

# Download the binary
try {
    $ProgressPreference = 'SilentlyContinue'
    Invoke-WebRequest -Uri $DownloadUrl -OutFile $TmpFile -UseBasicParsing
} catch {
    Write-Host "Failed to download binary from $DownloadUrl" -ForegroundColor Red
    if (Test-Path -Path $TmpFile) {
        Remove-Item -Path $TmpFile -Force
    }
    exit 1
}

# Verify checksum if available
$ChecksumUrl = "https://github.com/$GitHubOwner/$GitHubRepo/releases/download/$Tag/$AssetName.sha256"
try {
    $ChecksumResponse = Invoke-WebRequest -Uri $ChecksumUrl -UseBasicParsing -ErrorAction SilentlyContinue
    if ($ChecksumResponse.StatusCode -eq 200) {
        Write-Host "Verifying checksum..." -ForegroundColor Cyan
        $ChecksumFile = Join-Path $env:TEMP "$AssetName.sha256.tmp"
        $ChecksumResponse.Content | Out-File -Path $ChecksumFile -Encoding ASCII
        
        $DownloadedHash = (Get-FileHash -Path $TmpFile -Algorithm SHA256).Hash
        $ExpectedHash = ($ChecksumFile | Get-Content).Split()[0]
        
        if ($DownloadedHash -ne $ExpectedHash) {
            Write-Host "Checksum verification failed!" -ForegroundColor Red
            Remove-Item -Path $TmpFile -Force
            Remove-Item -Path $ChecksumFile -Force
            exit 1
        }
        
        Remove-Item -Path $ChecksumFile -Force
        Write-Host "Checksum verified" -ForegroundColor Green
    }
} catch {
    Write-Host "Warning: Could not verify checksum" -ForegroundColor Yellow
}

# Move binary to install directory
$TargetPath = Join-Path $InstallDir $BinaryName
Move-Item -Path $TmpFile -Destination $TargetPath -Force

Write-Host "Installation complete!" -ForegroundColor Green
Write-Host "Binary installed to: $TargetPath" -ForegroundColor Cyan

# Check if install directory is in PATH
$PathEnv = [Environment]::GetEnvironmentVariable("Path", "User")
if ($PathEnv -like "*$InstallDir*") {
    Write-Host "$InstallDir is in your PATH" -ForegroundColor Green
} else {
    Write-Host "Adding $InstallDir to PATH..." -ForegroundColor Cyan
    try {
        [Environment]::SetEnvironmentVariable(
            "Path",
            "$PathEnv;$InstallDir",
            "User"
        )
        Write-Host "Added to PATH" -ForegroundColor Green
        Write-Host "Note: You may need to restart your terminal for the changes to take effect" -ForegroundColor Cyan
    } catch {
        Write-Host "Failed to update PATH: $_" -ForegroundColor Yellow
        Write-Host "Please manually add $InstallDir to your PATH" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "To update in the future, run: shiden update" -ForegroundColor Cyan
Write-Host "To check for updates without installing: shiden update --check" -ForegroundColor Cyan
