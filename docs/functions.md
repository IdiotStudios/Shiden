# Shiden — Functions

Functions are declared with `fn new` and closed with `fn/`.

## Defining functions

A function body is a list of slash-terminated statements.
The function name becomes the symbol used for calls.

```shiden
fn new main/
    println("hello")/unit
fn/
```

## Parameters and return types

Parameters can include `/type`, and return type is written after the header.
If no return type is given, it defaults to `unit`.

```shiden
fn new add(a/i64, b/i64)/i64
    return a + b/i64
fn/
```

## Multi-word names

Function names can be written as multiple identifiers and are joined with `_`.
This is how module-style names like `fs read` are formed.

```shiden
fn new fs read(path)/str
    __fs_read(path)/str
fn/
```
