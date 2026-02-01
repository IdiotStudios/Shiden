# Shiden

Shiden is a high-performance, ahead-of-time compiled programming language focused on speed, predictability, and low-level control while retaining a familiar, Rust like syntax.

Shiden has a .sd file format

VSCode-Extension: [here](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)

---

## Language features (currently supported)

Shiden currently implements the following language features (stable enough for examples and tests):

- **Syntax & core**: Lexer, recursive-descent parser, AST, and a tree-walk interpreter.
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