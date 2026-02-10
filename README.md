# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language built for speed, predictability, and low-level control while retaining a familiar, Rust like syntax. (❤️ [Rust](https://github.com/rust-lang/rust))

Shiden has a .sd file format

VSCode-Extension: [here](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)  
Flavortown-Project: [here](https://flavortown.hackclub.com/projects/10866)
Browser-Extension: (hint...)

## Libraries

Shiden has built in [libraries](tree/main/src/libraries).  
Which are:

* [Networking](tree/main/src/libraries/networking)
* [Filesystem](tree/main/src/libraries/filesystem)


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
├── cache/              // Cache  
└── README.md  
```

shiden.toml:
```toml
  [package]
  name = "my_project"
  version = "0.1.0"
  license = "MIT"

  [build]
  # 0-3
  opt_level = 3
  targets = ["x86_64-linux", "x86_64-windows"]
```

## Contributing

If you do wanna contribute <3 then feel free to create issues or pull requests! I love the support.

## License

Licensed under the MIT License. See the `LICENSE` file for details.