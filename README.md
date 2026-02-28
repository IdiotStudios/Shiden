# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language built for speed, predictability, and low-level control while retaining a familiar, Rust like syntax. (❤️ [Rust](https://github.com/rust-lang/rust))

Shiden has a .sd file format

## AI Notice

Used AI for inline code completions and for documentation writing.

## Some things we have

[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)  
[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)  
[Shiden-Docs](https://shiden.arson.dev/docs)  
[Shiden-Hosting]() (hint...)  
[Shiden-Playground]() (hint...)  
[Shiden-Package-Registry]() (hint...)  
[Benchmarks]() (hmm very soon)  

## Todo

- Improved error messages with code snippets and suggestions
- Built-in libraries for common tasks (networking, filesystem, etc.)
- Better Documentation and more examples
- Unsafe code blocks for low-level operations
- Better testing framework and support for unit/integration tests (future plan)
- IDE integration and language server support (future plan)
- Performance optimizations and benchmarking tools (future plan)
- Support for more data types and features (enums, traits, async/await, etc.) (future plan)

## Libraries

Shiden has built in [libraries](src/libraries).  
Which are:

* [Filesystem](src/libraries/filesystem)

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
pub const add: &[u8] = &[
    0x55,                   // push rbp
    0x48, 0x89, 0xe5,       // mov rbp, rsp
    0x48, 0x89, 0xf8,       // mov rax, rdi
    0x48, 0x01, 0xf0,       // add rax, rsi
    0x5d,                   // pop rbp
    0xc3,                   // ret
];
```

## Multi-Platform Compilation

Shiden supports compiling for multiple platforms. The compiler automatically detects your current platform and builds for it, with the ability to cross-compile to other targets.

### Supported Platforms

* **Linux** (`linux`) - Implemented
* **Windows** (`windows`) - Implemented (beta)
* **macOS** (`macos`) - Planned

### Soon to be Supported

* **WebAssembly** (`wasm`)
* **ARM** (`arm`)
* **32-bit x86** (`x86`)
* **RISC-V** (`riscv`)
* **PowerPC** (`ppc`)
* **Mobile (iOS/Android)** (`mobile`)
* **Operating System** (`iso`)

### Configure Targets

Specify your target platforms in `shiden.toml`:

```toml
[build]
opt_level = 3
targets = ["linux", "windows", "macos"]
```

## CLI

* Shiden parse (parses shiden stuff)
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

If you do wanna contribute <3 then feel free to create issues or pull requests! I love the support.

## License

Licensed under the MIT License. See the `LICENSE` file for details.

## Extra

[code-typing](https://github.com/user-attachments/assets/f92badf9-a22e-46d5-b399-7c2127d3201a)
