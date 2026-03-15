# Shiden

![License](https://img.shields.io/github/license/IdiotStudios/Shiden)
![Stars](https://img.shields.io/github/stars/IdiotStudios/Shiden)
![Issues](https://img.shields.io/github/issues/IdiotStudios/Shiden)

Shiden is a high-performance ahead-of-time compiled language focused on speed, predictability, and low-level control, with a syntax familiar to Rust and JavaScript.

## NOTE:
Shiden is in early development and is not yet stable. Expect breaking changes and missing features. Contributions and feedback are welcome! Also I am in the process of rewriting teh rust implementation in ASM so expect some instability and missing features for a while. Release [v0.1.2](https://github.com/IdiotStudios/Shiden/releases/tag/v0.1.2) is the stable rust release, but the asm release is still in progress.

## To the Shipwright reviewing my project
The repo is being refactored so just ignore the assembly for now.Also you can run Shiden by downloading the relevant files for your OS from gh releases v0.1.2. The exe/binary are cli tools not GUI tools

## Features

- Ahead-of-time compilation
- Familiar Rust/JavaScript-like syntax
- Built-in low-level system primitives
- Built-in libraries implemented as optimized x86-64 machine code
- Fast startup and predictable performance

## Why Shiden?

Shiden is designed for developers who want the performance of systems languages without the complexity that often comes with them.

It focuses on:

- predictable performance
- simple syntax
- direct access to system primitives
- minimal runtime overhead

### Goal
Shiden aims to combine the performance and control of Rust with the simplicity of scripting languages while exposing low-level system primitives directly.

### Platforms

| Platform | Arch | Support | Phase |
| :--- | :---: | :---: | ---: |
| `linux` | x86_64 | No | N/A |
| `linux` | arm | No | N/A |
| `windows` | x86_64 | No | N/A |
| `macos` | N/A | No | N/A |
More architectures and platforms are planned.

### Random Thought
GitHub should add polls to repos. That would be peak.

## Installation

### Going to be added soon

## Ecosystem

- 🧠 **[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)**  
- 🍽️ **[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)**  
- 📚 **[Shiden-Docs](https://docs.shiden.kirze.de/)**  
- 📦 **[Shiden-Package-Registry]()** (hint...)  
- 🧪 **[Benchmarks]()** (hmm very soon)  
(thanks kde emoji picker)

## Cli - Rust version
| Command | Description | Status |
| :--- | :--- | :---: |
| `shiden run` | release mode by default, add `--debug` for debug mode | ✅ |
| `shiden check` | runs checks | ✅ |
| `shiden new {name}` | creates a new project | ✅ |
| `shiden compile` | compiles Shiden (run does the same anyway) | ✅ |
| `shiden help` | shows help | ✅ |

## CLI - Assembly version
| Command | Description | Status |
| :--- | :--- | :---: |
| `shiden run` | release mode by default, add `--debug` for debug mode | ✅ |
| `shiden check` | runs checks | ✅ |
| `shiden new {name}` | creates a new project | ✅ |
| `shiden init` | initializes project in current directory | ✅ |
| `shiden compile` | compiles Shiden (run does the same anyway) | ✅ |
| `shiden help` | shows help | ✅ |

### Shiden init/new folder formats

```
my_project/  
├── shiden.toml         // Manifest  
├── src/  
│   └── main.sd         // Executable entry  
├── tests/  
│   └── example.sd  // doesn't work yet  
├── build/              // Compiled binaries  
│   ├── cache/   // Build cache (none)  
│   ├── linux/   // Linux target  
│   ├── windows/ // Windows target  
│   └── macos/   // macOS target  
└── README.md  
```

## Supporters

Shiden is a project supported by [GitBook](https://www.gitbook.com/?utm_source=content&utm_medium=sponsored-by-gitbook&utm_campaign=xaAHE1MNwN8fiIAs9Mux)! If you want to support the project then consider sponsoring or donating! It really helps a lot and is very much appreciated.

## Contributing

If you do wanna contribute then feel free to create issues or pull requests! Love the support.
Also Checkout the [CONTRIBUTING.md](CONTRIBUTING.md) for more details on how to contribute.

## License

Licensed under the MIT License. See the `LICENSE` file for details.

## AI Notice

Used AI for inline code completions and for documentation writing. Also for debugging.