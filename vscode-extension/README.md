# Shiden VSCode Extension — Formatter

Provides a simple formatter for Shiden `.sd` files, plus syntax highlighting and snippets.

Features
- Format document command: `Shiden: Format Document`
- Format on save (enabled by default, controlled by `shiden.format.onSave`)
- Diagnostics: parse/format checks that show errors in the Problems view (configurable via `shiden.diagnostics.*`).
  - Optional: run `shiden check` CLI on save (enable `shiden.diagnostics.useCli`).
- Linting heuristics: detects missing `main`, unused/duplicate functions, and provides quick fixes.
- Code actions / quick fixes for common issues (append `/unit`, replace tabs, fix indentation, close `fn/` blocks).
- Snippets for functions, `if`/`else`, `while`, `for`, array operations, and file I/O (`read_file`, `write_file`).
- Context-aware completions
- Hover: shows simple function signatures inferred from `fn new` headers.
- Signature help: shows parameter hints when typing function calls.

Notes
- The formatter mirrors the rules in `src/format/mod.rs` (4-space indentation, no tabs, statements ending with `/`, no semicolons, no inline `//` comments outside strings).

How to build
1. cd vscode-extension
2. vsce package