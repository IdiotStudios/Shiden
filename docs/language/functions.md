# Shiden — Functions

Functions are declared with `fn new` and closed with `fn/`.

## Defining functions

A function body is a list of slash-terminated statements.
The function name becomes the symbol used for calls.

```shiden
fn new main/
    println("hello")/
fn/
```

## Parameters

Parameters are just identifiers separated by commas.
Types are inferred from usage or can be specified at call sites with let bindings.

```shiden
fn new add(a, b)/
    return a + b/
fn/
```

## Multi-word names

Function names can be written as multiple identifiers and are joined with `_`.
This is how module-style names like `fs read` are formed.

```shiden
fn new fs read(path)/
    let result = __fs_read(path)/str
    return result/
fn/
```
