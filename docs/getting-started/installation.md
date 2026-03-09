# Installation

Shiden can be installed via Crates or using pre-built binaries with automatic updates.

## Option 1: From Cargo

If you have Rust installed, you can install Shiden directly from crates.io:

```bash
cargo install shiden
```

To update, just run the same command again.

## Uninstalling

### From Cargo

```bash
cargo uninstall shiden
```

## Supported Platforms

* Linux x86\_64
* Linux ARM64
* Windows x86\_64

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

