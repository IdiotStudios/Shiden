# Installing Shiden

Shiden can be installed via Crates or using pre-built binaries with automatic updates.  

## Option 1: From Cargo

If you have Rust installed, you can install Shiden directly from crates.io:

```bash
cargo install shiden
```
  
To update, just run the same command again.

## Option 2: Pre-built Binaries

### Linux & macOS

Run the installation script:

```bash
curl -fsSL https://raw.githubusercontent.com/IdiotStudios/Shiden/main/install.sh | bash
```

Or download and run manually:

```bash
bash install.sh
```

The script will:
- Detect your OS and architecture automatically
- Download the appropriate binary from GitHub releases
- Verify the checksum
- Install to `~/.local/bin` (or custom `$INSTALL_DIR`)
- Add to your PATH if needed

### Windows

Run the installation script in PowerShell:

```powershell
powershell -ExecutionPolicy Bypass -File install.ps1
```

Or using a one-liner for remote execution:

```powershell
iex (iwr https://raw.githubusercontent.com/IdiotStudios/Shiden/main/install.ps1 -UseBasicParsing).Content
```

The script will:
- Download the appropriate binary
- Verify the checksum
- Install to `%USERPROFILE%\.shiden\bin`
- Add to your PATH automatically

## Updating

Once installed via pre-built binaries, updating is easy:

```bash
shiden update
```

Check for updates without installing:

```bash
shiden update --check
```

## Uninstalling

### From Cargo

```bash
cargo uninstall shiden
```

### From Pre-built Binaries

Simply remove the binary:

```bash
# Linux/macOS
rm ~/.local/bin/shiden

# Windows
del %USERPROFILE%\.shiden\bin\shiden.exe
```

And optionally remove the install directory:

```bash
# Linux/macOS
rm -rf ~/.local/bin/shiden

# Windows  
rmdir %USERPROFILE%\.shiden
```

## Supported Platforms

- Linux x86_64
- Linux ARM64
- macOS x86_64
- macOS ARM64 (Apple Silicon)
- Windows x86_64

## Verifying Installation

Verify the installation by checking the version:

```bash
shiden --version
```

## Troubleshooting

### "Command not found" on Linux/macOS

If you get "command not found: shiden", make sure `~/.local/bin` is in your PATH.

Add this to your shell profile (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
export PATH="$HOME/.local/bin:$PATH"
```

Then reload your shell:

```bash
source ~/.bashrc  # or ~/.zshrc
```

### "Command not found" on Windows

If you get "'shiden' is not recognized", restart your terminal or command prompt after installation. The PATH update requires a restart to take effect.

### Checksum Verification Failed

Try downloading the binary manually from the [releases page](https://github.com/IdiotStudios/Shiden/releases).

### Build from Source

If you prefer to build from source:

```bash
git clone https://github.com/IdiotStudios/Shiden.git
cd Shiden
cargo build --release
./target/release/shiden --version
```

## Getting Help

For issues or questions:
- Check the [documentation](https://shiden.arson.dev/docs/)
- Open an [issue on GitHub](https://github.com/IdiotStudios/Shiden/issues)
- Check the [troubleshooting guide](https://shiden.arson.dev/docs/#troubleshooting) in the docs
