# LowShiden

LowShiden is the low-level subset of Shiden: a readable, explicit assembly-like source format (.sd) that compiles to native code across platforms.

Goals
- Provide a small, well-defined low-level language that is easy to parse and reason about.
- Keep sources portable: a LowShiden program compiles for multiple targets without source changes.
- Keep the compiler implementation minimal and dependency-free: the compiler is written in Rust with no external crates.
- Enable bootstrapping: LowShiden should be able to express the primitives needed to implement the compiler and runtime.
- Easily add more compiling outputs to LowShiden even after Self Hosting is achieved.

Constraints
- All source files use the `.sd` extension and follow existing Shiden rules (slash-terminated statements, `fn` blocks, type suffixes where appropriate).
- Compiler implementation uses no Rust dependencies (std is allowed); everything else is implemented in-house.
- The runtime/target-specific code is kept small and isolated behind a thin ABI layer.

File layout and conventions
- File extension: `.sd`
- Statements are terminated with `/` and blocks use `fn new .../` and `fn/` markers (see `docs/language/syntax.md`).
- LowShiden favors explicitness: all memory, types, and control flow are spelled out.
- Only `let` bindings are allowed for variables in LowShiden. Type suffixes (the `/<type>` tokens) are required only on `let` statements and should appear immediately after the binding; calls and other statements do not carry type suffixes.

Language surface (overview)
- Modules & imports: use the same top-level import rules as Shiden.
- Functions: `fn new NAME(args)/` ... `fn/` — LowShiden functions are explicit about stack/heap effects.
 - Types: Use Shiden postfix types (e.g., `/u64`, `/i32`, `/str`). LowShiden introduces a small set of primitive types for codegen (integers, pointers). Type suffixes are only attached to `let` bindings; do not append type suffixes to calls or to `ret`/other statements.
- Labels: Use label declarations for local control flow: `label name/` and `jmp name/` to jump. Labels are file-local.

Primitives and host calls
- Memory primitives: `alloc(size)/ptr`, `free(ptr)`, `load(ptr, offset)/u8`, `store(ptr, offset, val)` — these map to platform runtime helpers. Note: effectful calls like `free` and `store` are statements terminated with `/` and do not require a `/unit` suffix.
- I/O primitives: Minimal helper calls (print, read) should be implemented as small runtime helper binaries and invoked via well-known names (see `docs/helpers.md`).
- Syscalls/platform calls: Target-specific implementations should live under `libraries/runtime/<target>/` and be invoked through a stable ABI.

Control flow
- Conditional branches: `if <cond>/bool then_label else_label/` — or explicit compare+jmp pairs.
- Direct jumps: `jmp label/`.
- Function calls: `call label/` and `ret/` with explicit stack discipline described below. Calls are written as `call name(args)/` and do not carry type suffixes on the call itself; any returned values must be bound with `let` using a type suffix if a type is required.

Calling convention and stack model
- Explicit stack-frame model: callers push arguments and a return address; callees are responsible for preserving designated caller-saved slots.
- Stack grows downward; pointers are machine-word sized for the target (`usize`).
- All pointer/word values should be annotated with `/usize`.
- ABI is target-aware: the compiler provides a small platform layer to translate LowShiden frame ops to native prologue/epilogue.

Memory model
- Flat linear address space per process.
- Data layouts are little-endian by default; target backends must document differences.
- Alignment rules: word-sized values aligned to machine word; smaller types packed with explicit offsets.

Cross-platform strategy
- Keep high-level IR and LowShiden semantics platform-independent.
- Implement platform-specific lowering in the compiler backends (e.g., x86_64, aarch64). Backends are small, isolated modules.
- Use a small, documented ABI boundary: runtime helpers and auto-generated helper binaries (see `docs/helpers.md`) implement OS-specific behavior.

Compiler responsibilities
- Frontend: tokenization and parsing of `.sd` into an explicit AST.
- Validation: type checks for LowShiden subset and stack/frame sanity checks.
- IR: Lower parsed AST to a small, target-agnostic IR that expresses control flow, memory ops, and calls.
- Codegen: per-target code generator that emits object/executable or calls an assembler/linker.
- No external crates: implement lexer, parser, IR, and codegen utilities directly in Rust.

CLI and build
- Recommended CLI subcommands (to be implemented in `compiler/`):
  - `shiden low parse <file>.sd` → prints AST/IR
  - `shiden low build --target <triple> <file>.sd` → emit object/exe for target
  - `shiden low run <file>.sd --target <triple>` → build and run locally (where possible)

Examples (minimal)

A very small LowShiden program that allocates and writes a byte:

```shiden
fn new main/
  let p = alloc(8)/usize
  store(p, 0, 42)/
  call print_u8(p, 0)/
  ret/
fn/

fn new print_u8(ptr, off)/
  let v = load(ptr, off)/u8
  print(v)/
  ret/
fn/
```

Bootstrapping and self-hosting
- Start by implementing the parser and a simple codegen backend that emits a native binary for the host platform.
- Provide a minimal runtime for the host (heap alloc, print helpers) under `libraries/runtime/<target>/`.
- Gradually implement more backends and use LowShiden to express parts of the compiler/runtime itself.

Notes and next steps
- This document captures the initial LowShiden surface and constraints. It references existing Shiden language rules in `docs/language/*.md` and helper binaries in `docs/helpers.md`.
- Next: scaffold the `compiler` CLI and a parser module that recognizes the LowShiden subset.

