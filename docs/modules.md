# Shiden — Modules and Imports

Imports bind a path to a name that you can call later.

## Import syntax

Use `let` with `/import` at the top level.
The import path is a string literal, not a file system path.

```shiden
let "brainfuck" = "bf"/import
```

## Import alias usage

The alias becomes the prefix for functions provided by the module.
Aliases are plain identifiers and can be used in multiple files.

```shiden
let "brainfuck" = "bf"/import

fn new main/
    bf_run("+[]")/unit
fn/
```
