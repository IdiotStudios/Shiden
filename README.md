# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language focused on speed, predictability, and low-level control while retaining a familiar, Rust like syntax.

Shiden has a .sd file format

VSCode-Extension: [here](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)

---

## Language features (currently supported)

Shiden currently implements the following language features (stable enough for examples and tests):

- **Syntax & core**: Lexer, recursive-descent parser, AST, and an ahead-of-time compiler backend.

## Compiler & Build pipeline

Shiden compiles `.sd` programs into native x86_64 ELF binaries (Linux) using an internal pipeline:
(Working on Windows and macos compilation next)

1. **Parse** — `frontend::parse(src)` → `Program` (AST).
2. **Lowering** — AST → small stack-based IR (push/pop, binops, control flow, calls/ret).
3. **Codegen** — IR → text section bytes (machine code), uses `object` crate to produce a relocatable object.
4. **Assemble & Link** — invoke `as` and `ld` to produce the final executable.

Important implementation notes:
- SystemV x86_64 calling convention is used for function calls (registers: `rdi, rsi, rdx, rcx, r8, r9`, extras on stack).
- The compiler supports spilling arguments past the register count and maintains 16-byte alignment.

Usage:

```bash
# compile a project in the current directory (expects shiden.toml)
shiden compile

# compile and run a single source file (temporary project)
shiden run examples/hello.sd
```

Contributing & Development

- Tests: `cargo test` — the test suite contains many end-to-end cases that compile test programs, run the produced executable, and assert stdout.
- Build: the build backend lives in `src/build/mod.rs` (lowering + codegen).
- **Types & values**: `i64`, `f64`, `char`, `string`, `array`, `unit`, `bool`.
- **Expressions & operators**:
  - Arithmetic: `+`, `-`, `*`, `/`.
  - Comparisons: `==`, `!=`, `<`, `<=`, `>`, `>=`.
  - Logical: `&&`, `||`, unary `!`.
  - Unary negation: `-`.
  - Indexing: `a[index]`.
  - Function calls and arguments.
- **Statements & control flow**:
  - `let` (with optional `mut`), assignments, `return`.
  - `if` / `else`, `while`, `for <var> in <iterable>`.
  - `break`, `continue`.
- **Collections & builtins**:
  - Arrays and array operations: `push`, `pop`, `len`, `range`.
  - Builtins: `println`, `print`, file `read_file`/`write_file` (where available).
- **Additional details**:
  - Typed terminators like `/i64` or a plain `/` are used to end many statements and declarations.
  - Example programs live in the `examples/` directory, see `examples/brainfuck.sd` (a Brainfuck interpreter written in Shiden).

---

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

my_project/  
├── shiden.toml        // Manifest  
├── src/  
│   └── main.sd        // Executable entry  
├── tests/  
│   └── example.sd  
├── build/             // Compiled binaries  
├── cache/             // Cache  
└── README.md  

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

---
## Contributing

If you do wanna contribute (apprciated) then feel free to create issues or pull requests! I love the support.

---

## License

Licensed under the MIT License. See the top-level `LICENSE` file for details.