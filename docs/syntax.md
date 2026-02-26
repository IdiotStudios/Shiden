# Shiden — Syntax

Shiden uses slash-terminated statements and block markers instead of braces.

For a complete working example of all syntax features, see `examples/docs/` in the repository.

## File structure

Top-level items are imports and function definitions.
Imports must appear before functions so names resolve cleanly.

```shiden
let "brainfuck" = "bf"/import

fn new main/
    println("hello")/unit
fn/
```

## Statement terminators

Every statement ends with `/`.
Only `let` statements attach a `/<type>` suffix for type binding.

```shiden
let msg = "hi"/str
println("{}", msg)/
```

## Function blocks

Functions open with `fn new` and close with `fn/`.
The close marker is required even for single-line bodies.

```shiden
fn new greet(name)/
    println("Hello {}", name)/
fn/
```

## Imports

Imports use `let "path" = alias/import` at the top level.
The alias becomes a prefix for module functions.

```shiden
let "filesystem" = fs/import

fn new main/
    println("{}", fs_read("/tmp/a"))/
fn/
```
