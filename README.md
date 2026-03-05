# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language built for speed, predictability, and low-level control while retaining a familiar, Rust and JavaScript like syntax. (❤️ [Rust](https://github.com/rust-lang/rust))


| Plaform | Support | Phase |
| :--- | :---: | ---: |
| `linux` | Yes | Stable |
| `windows` | Yes | Beta |
| `macos` | No | Planned |
| `arm` | No | Planned |

### Side Note:
They should add like polls to gh repos. Would be Peak


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

## Some things we have

[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)  
[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)  
[Shiden-Docs](https://shiden.arson.dev/docs)  
[Shiden-Package-Registry]() (hint...)  
[Benchmarks]() (hmm very soon)  

## Todo

1. Improved error messages with code snippets and suggestions
2. Unsafe code blocks for low-level operations
3. Better testing framework and support for unit/integration tests
4. Performance optimizations and benchmarking tools
5. Support for more data types and features (enums, traits, async/await, etc.)

## Libraries

Shiden has built in [libraries](src/libraries).  
Best ones are:

* [Filesystem](src/libraries/filesystem)
* [Networking](src/libraries/networking)


### Library Architecture

Libraries provide helper functions for common tasks. Each library contains hand-written x86-64 bytecode for maximum performance.

**Bytecode Format:**
Library functions are implemented as raw x86-64 machine code embedded as `Vec<u8>` in Rust.

To add a library function:
1. Create `src/libraries/xyz/mod.rs`
2. Define your function as a public constant: `pub const FUNCTION_NAME: &[u8] = &[/* x86-64 bytecode */];`
3. Export it in the library's module

**Example:**
```rust
// Simple add function in x86-64 bytecode
// FYI I would not trust my bytecode so use this as an example
pub const add: &[u8] = &[
    0x55,                   // push rbp
    0x48, 0x89, 0xe5,       // mov rbp, rsp
    0x48, 0x89, 0xf8,       // mov rax, rdi
    0x48, 0x01, 0xf0,       // add rax, rsi
    0x5d,                   // pop rbp
    0xc3,                   // ret
];
```

## CLI

* Shiden parse (parses shiden stuff) - deprecating
* Shiden run (release mode by default, add --debug for debug mode)
* Shiden check (runs checks)
* Shiden new {name} (makes an new project)
* Shiden compile (Compiles Shiden (Shiden run does the same anyway))
* Shiden help (shows a help command)

### Shiden init/new folder formats

```
my_project/  
├── shiden.toml         // Manifest  
├── src/  
│   └── main.sd         // Executable entry  
├── tests/  
│   └── example.sd  // doesent work yet  
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