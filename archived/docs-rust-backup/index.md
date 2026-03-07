# Shiden — Documentation

Welcome to the Shiden docs — concise reference and quick start for the Shiden programming language and compiler. This is a work in progress, so expect some gaps and rough edges. For questions or contributions, see the [contact page](https://shiden.arson.dev/contact).

## Contents

- **CLI**: command-line front end and subcommands (`parse`, `run`, `check`, `new`)
- **Compiler**: Ahead-of-time compilation pipeline (AST → IR → object → exe) — see `docs/build.md`
- **Frontend (parser)**: `parse(src: &str) -> Result<Program, String>` — produces the AST
- **Language**: syntax (`docs/syntax.md`), types (`docs/types.md`), variables (`docs/variables.md`), functions (`docs/functions.md`), expressions (`docs/expressions.md`), control flow (`docs/control-flow.md`), modules (`docs/modules.md`)
- **Numeric types**: integers with generic bit-widths (i1-i128, u1-u128) and floating-point support (f16, f32, f64, f80, f128) — see `docs/types.md` and `docs/floats.md`
- **Format**: `check_format` — source style rules (see `docs/format.md` and `docs/formatting.md`)
- **Standard library**: filesystem and networking helpers (`docs/standard-library.md`)
- **Timing**: high-resolution platform-specific timers for compile and execution times (`docs/timing.md`)

## Quick start

- Build/tests: `cargo test`
- Format: `cargo fmt`
- Run: `cargo run -- run path/to/file.sd` or use `shiden run` from the CLI

## Examples

Check out `examples/types/` for a comprehensive demonstration of all 267+ supported types. Run it with:

```bash
cargo run -- run examples/types/
```

For complete language feature examples, see `examples/docs/`. Other examples are `examples/HelloWorld/`, `examples/brainfuck/`, and `examples/http server/`.

---