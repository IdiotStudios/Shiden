# Shiden — Documentation

Welcome to the Shiden docs — concise reference and quick start for the Shiden programming language and compiler. This is a work in progress, so expect some gaps and rough edges. For questions or contributions, see the [contact page](https://shiden.arson.dev/contact).

## Contents

- **CLI**: command-line front end and subcommands (`parse`, `run`, `check`, `new`)
- **Compiler**: Ahead-of-time compilation pipeline (AST → IR → object → exe) — see `docs/build.md`
- **Frontend (parser)**: `parse(src: &str) -> Result<Program, String>` — produces the AST
- **Syntax**: tokens and AST types (`Program`, `Item`, `Stmt`, `Expr`)
- **Format**: `check_format` — source style rules (see `docs/format.md` and `docs/formatting.md`)
- **Libraries**: `libraries/filesystem` — convenience wrappers for file operations

## Quick start

- Build/tests: `cargo test`
- Format: `cargo fmt`
- Run: `cargo run -- run path/to/file.sd` or use `shiden run` from the CLI

---