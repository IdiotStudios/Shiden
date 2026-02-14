# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language built for speed, predictability, and low-level control while retaining a familiar, Rust like syntax. (❤️ [Rust](https://github.com/rust-lang/rust))

Shiden has a .sd file format

VSCode-Extension: [here](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)  
Flavortown-Project: [here](https://flavortown.hackclub.com/projects/10866)  
Browser-Extension: (hint...)

## Libraries

Shiden has built in [libraries](src/libraries).  
Which are:

* [Networking](src/libraries/networking)
* [Filesystem](src/libraries/filesystem)

## Multi-Platform Compilation

Shiden supports compiling for multiple platforms. The compiler automatically detects your current platform and builds for it, with the ability to cross-compile to other targets.

### Supported Platforms

* **Linux** (`x86_64-linux`) - Implemented
* **Windows** (`x86_64-windows`) - Next up
* **macOS** (`x86_64-macos`) - Planned (after Windows)

### Configure Targets

Specify your target platforms in `shiden.toml`:

```toml
[build]
opt_level = 3
targets = ["x86_64-linux", "x86_64-windows", "x86_64-macos"]
```

## CLI

CLI NAME: Shiden

### Commands:

* Shiden new {name} (makes an new project)
* Shiden init (makes a new project)
* Shiden check (checks code for errors — supports `--format json` for structured diagnostics)
* Shiden run (release mode by default, add --debug for debug mode)
* Shiden build (release mode by default, add --debug for debug mode)
* Shiden clean (removes build artifacts)
* Shiden format (formats code)
* Shiden tests (runs tests [future plan])

### Shiden init/new folder formats

```
my_project/  
├── shiden.toml         // Manifest  
├── src/  
│   └── main.sd         // Executable entry  
├── tests/  
│   └── example.sd  
├── build/              // Compiled binaries
│   ├── cache/   // Build cache
│   ├── x86_64-linux/   // Linux target
│   ├── x86_64-windows/ // Windows target
│   └── x86_64-macos/   // macOS target
└── README.md  
```

shiden.toml:
```toml
  [project]
  name = "my_project"
  version = "0.1.0"
  license = "MIT"

  [build]
  opt_level = 3
  # Target platforms to compile for
  targets = ["x86_64-linux", "x86_64-windows", "x86_64-macos"]
```

## Contributing
If you do wanna contribute <3 then feel free to create issues or pull requests! I love the support.

## License

Licensed under the MIT License. See the `LICENSE` file for details.