# Shiden VSCode Extension — Formatter

Provides a simple formatter for Shiden `.sd` files, plus syntax highlighting and snippets.

Features
- Format document command: `Shiden: Format Document`
- Format on save (enabled by default, controlled by `shiden.format.onSave`)

Notes
- The formatter mirrors the rules in `src/format/mod.rs` (4-space indentation, no tabs, statements ending with `/`, no semicolons, no inline `//` comments outside strings).

How to build
1. cd vscode-extension
2. vsce package