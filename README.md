# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language built for speed, predictability, and low-level control while retaining a familiar, Rust like syntax. (❤️ [Rust](https://github.com/rust-lang/rust))

Shiden has a .sd file format

## AI Notice

Used AI for inline code completions and for documentation writing.

## Some things we have

[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)  
[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)  
[Shiden-Docs](https://shiden.arson.dev/docs)  
[Browser-Extension]() (hint...)  
[Shiden-Hosting]() (hint...)  
[Shiden-Playground]() (hint...)  
[Shiden-LLM-Code-Assistant]() (hint...)  
[Shiden-Package-Registry]() (hint...)  
[Benchmarks]() (hmm very soon)  

## Todo

- Improved error messages with code snippets and suggestions
- Support for more platforms (Windows, macOS)
- Built-in libraries for common tasks (networking, filesystem, etc.)
- Documentation and more examples
- Unsafe code blocks for low-level operations
- Better testing framework and support for unit/integration tests (future plan)
- IDE integration and language server support (future plan)
- Performance optimizations and benchmarking tools (future plan)
- Split the compiler into multiple stages (lexer, parser, semantic analysis, code generation) for better modularity and maintainability (future plan)
- Support for more data types and features (enums, traits, async/await, etc.) (future plan)
- A standard library for common utilities and data structures (future plan)
- A REPL for interactive coding and experimentation (future plan)
- A web-based playground for trying out Shiden code without installing anything (future plan)
- A LLM-based code assistant for generating code snippets and providing suggestions (future plan)
- A community-driven package registry for sharing and discovering libraries (future plan)
- ISO compilation target for OS development (future plan)
- WebAssembly target for running Shiden code in the browser (future plan)

## Libraries

Shiden has built in [libraries](src/libraries).  
Which are:

* [Networking](src/libraries/networking)
* [Filesystem](src/libraries/filesystem)
* [Math](src/libraries/math)

### Library Architecture

Libraries are automatically discovered and compiled. Each library can contain helper functions written in:

1. **Bytecode (Current):** x86-64 machine code embedded as vectors of bytes for maximum performance
2. **Rust via Bytecode Compiler:** Rust functions that are transpiled to x86-64 bytecode at compile time

To add a library function, create `src/libraries/xyz/lib.rs` or `src/libraries/xyz/mod.rs` with public functions. The Bytecode Compiler will discover and compile them automatically.

### Bytecode Compiler

The Shiden Bytecode Compiler (`src/compiler/`) is a custom Rust→x86-64 transpiler that compiles simple Rust functions into native machine code at build time.

**Supported Rust Subset:**
- Arithmetic operators: `+`, `-`, `*`, `/`, `%`
- Comparison operators: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logical operators: `&&`, `||`, `!`
- Control flow: `if`/`else`, `while` loops
- Variables with `let` and `mut`
- Function parameters and return values (i64 only)
- Function calls to other helper functions
- Arrays (via helper functions like `array_get`, `array_set`, `len`)
- **Structs** with fields and methods (Phase 3-4)
- **Methods** with `self` parameter and method chaining (Phase 4)
- **Associated Functions** (static methods) with `Type::function()` syntax (Phase 5)
- **Return Values** from methods using `Self` type and explicit `return` statements (Phase 6)
- **Mutable Self** with field assignments (Phase 7)
- **Traits** - trait definitions with method signatures and trait implementations (Phase 8)
- **Enums** with tagged variants (Phase 3)

**Unsupported:**
- String literals beyond what's hard-coded
- Generics
- Closures or higher-order functions
- Complex pattern matching
- Trait bounds and generic trait implementations
- Associated types
- Default trait implementations

**Examples:**

Associated Functions (Constructors):
```rust
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn new(x: i64, y: i64) -> Point {
        Point { x: x, y: y }
    }
}

pub fn create_point() -> i64 {
    Point::new(10, 20).x
}
```

Method Chaining with Return Values:
```rust
impl Point {
    fn move_by(self, dx: i64, dy: i64) -> Point {
        Point { x: self.x + dx, y: self.y + dy }
    }
}

pub fn chain_operations() -> i64 {
    Point::new(0, 0)
        .move_by(10, 20)
        .move_by(5, 5)
        .x
}
```

Mutable Self & Field Assignments:
```rust
impl Point {
    fn reset(&mut self) {
        self.x = 0;
        self.y = 0;
    }
}
```

Traits:
```rust
trait Display {
    fn show(self) -> i64;
}

impl Display for Point {
    fn show(self) -> i64 {
        self.x + self.y
    }
}

pub fn use_trait() -> i64 {
    Point::new(10, 20).show()
}
```

**Configuration:**
Stack allocation: 8-byte slots per local variable
Local limit: 256 per function (configurable via `BYTECODE_MAX_LOCALS`)
Division by zero: Traps (OS handles via exception)
Recursion: Supported
Optimization: Minimal (trivial inlines only)


## Multi-Platform Compilation

Shiden supports compiling for multiple platforms. The compiler automatically detects your current platform and builds for it, with the ability to cross-compile to other targets.

### Supported Platforms

* **Linux** (`x86_64-linux`) - Implemented
* **Windows** (`x86_64-windows`) - In progress (needs more work, especially around runtime args handling)
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
* Shiden compile (release mode by default, add --debug for debug mode)
* Shiden clean (removes build artifacts)
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

## Extra

[code-typing](https://github.com/user-attachments/assets/f92badf9-a22e-46d5-b399-7c2127d3201a)
