# Shiden

![License](https://img.shields.io/github/license/IdiotStudios/Shiden)
![Stars](https://img.shields.io/github/stars/IdiotStudios/Shiden)
![Issues](https://img.shields.io/github/issues/IdiotStudios/Shiden)

Shiden is a high-performance ahead-of-time compiled language focused on speed, predictability, and low-level control, with a syntax familiar to Rust and JavaScript.

Fast • Predictable • Minimal

#### Hello World — Example
(em-dashes ❤️)

```shiden
fn new main/
    println("Hello, world!")/
fn/
```

Run it with:
```bash
shiden run src/main.sd
```

#### Quick Example
```shiden
fn new main/

    let nums = [1,2,3,4]/

    for n in nums/
        println(n)/
    fn/

fn/
```

## Features

- Ahead-of-time compilation
- Familiar Rust/JavaScript-like syntax
- Built-in low-level system primitives
- Built-in libraries implemented as optimized x86-64 machine code
- Fast startup and predictable performance

## Benchmarks

All benchmarks are run on GitHub Actions and my local machine.  
My machine uses an Intel i7-4770K CPU with 16GB of RAM on Arch Linux.  
Benchmarks are averages based off scripts in the `benchmarks/` directory.  
All Benchmarks are avearge of 1000 runs.

### **THIS IS A WORK IN PROGRESS!**

### Hello World

| Environment | Shiden | Rust | Assembly | Node.js | Python |
|-------------|-----:|-----:|-----:|-----:|-----:|
| Local | ns | ns | ns | ms | ms |
| GitHub | ns | ns | ns | ms | ms |

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
| `linux` | x86_64 | Yes | Stable |
| `linux` | arm | No | Planned |
| `windows` | x86_64 | Yes | Beta |
| `macos` | N/A | No | Planned |
More architectures and platforms are planned.

### Random Thought
GitHub should add polls to repos. That would be peak.

## Installation

### Quick Start

**Linux/macOS:**
```bash
curl -fsSL https://raw.githubusercontent.com/IdiotStudios/Shiden/main/install.sh | bash
```

**Windows (PowerShell):**
```powershell
powershell -ExecutionPolicy Bypass -File (iwr https://raw.githubusercontent.com/IdiotStudios/Shiden/main/install.ps1 -UseBasicParsing).Content
```

**Crates.io:** (Will work in next release ~1-2d)
```bash
cargo install shiden
```

### Auto-Updates

Check for updates:
```bash
shiden update --check
```

Install latest version:
```bash
shiden update
```

See [INSTALL.md](https://shiden.arson.dev/docs/#installation) for detailed installation instructions and troubleshooting.

## Ecosystem

- 🧠 **[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)**  
- 🍽️ **[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)**  
- 📚 **[Shiden-Docs](https://shiden.arson.dev/docs)**  
- 📦 **[Shiden-Package-Registry]()** (hint...)  
- 🧪 **[Benchmarks]()** (hmm very soon)  
(thanks kde emoji picker)

## Todo

1. Improved error messages with code snippets and suggestions
2. Unsafe code blocks for low-level operations
3. Better testing framework and support for unit/integration tests
4. Performance optimizations and benchmarking tools
5. Support for more data types and features (enums, traits, async/await, etc.)

## Libraries

Shiden includes built-in libraries.
Some of the most useful include:

* [Filesystem](src/libraries/filesystem)
* [Networking](src/libraries/networking)


### Assembly Libraries

#### Auto Helpers

Building the project with the `auto-helpers` feature (enabled by default)
will automatically assemble any `*.asm` sources placed under
`libraries/<category>/<platform>/` and generate the Rust compiler code.

The build script (`build.rs`) also produces a documentation page at
`docs/helpers.md` listing all helpers and their binary locations.

To disable helper generation (e.g. for cross-building) remove the feature:

```sh
cargo build --no-default-features
```

#### Installs

Also Install nasm: (feel free to do a pr to update this)
```bash
sudo pacman -S nasm # Arch Linux
sudo apt install nasm # Debian/Ubuntu
```

Install asmfmt:
```bash
go install github.com/klauspost/asmfmt/cmd/asmfmt@latest

# add go to path if not already
echo 'export PATH="$HOME/go/bin:$PATH"' >> ~/.bashrc
```

Run it with
```bash
find libraries -name '*.asm' -exec asmfmt -w {} +
```

## CLI
| Command | Description | Status |
| :--- | :--- | :---: |
| `shiden parse` | parses Shiden files | deprecating |
| `shiden run` | release mode by default, add `--debug` for debug mode | ✅ |
| `shiden check` | runs checks | ✅ |
| `shiden new {name}` | creates a new project | ✅ |
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

## Contributing

If you do wanna contribute then feel free to create issues or pull requests! Love the support.

## License

Licensed under the MIT License. See the `LICENSE` file for details.

## AI Notice

Used AI for inline code completions and for documentation writing. Also for debugging.