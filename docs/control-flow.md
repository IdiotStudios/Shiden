# Shiden — Control Flow

Blocks use `if/`, `while/`, and `for/` to close, not braces.

See `examples/docs/src/main.sd` for complete control flow examples.

## if and else

Conditions end with `/` and blocks end with `if/`.
The `else/` block is optional and must appear before the closing `if/`.

```shiden
fn new main/
    if 1 == 1/
        println("yes")/
    else/
        println("no")/
    if/
fn/
```

## while

`while` repeats until its condition is false.
The loop body is closed with `while/`.

```shiden
fn new main/
    let mut x = 0/i64
    while x < 3/
        x = x + 1/i64
    while/
fn/
```

## for in

`for` iterates over an expression (often an array).
The loop variable is a new binding scoped to the loop body.

```shiden
fn new main/
    for n in [1, 2, 3]/
        println("{}", n)/
    for/
fn/
```

## break and continue

`break/` exits a loop and `continue/` skips to the next iteration.
Both are statements and end with `/`.

```shiden
fn new main/
    let mut x = 0/i64
    while x < 5/
        x = x + 1/i64
        if x == 3/
            continue/
        if/
        if x == 4/
            break/
        if/
    while/
fn/
```

## return

`return` ends a function and can include an expression.
The statement ends with `/` like any other non-let statement.

```shiden
fn new id(x)/
    return x/
fn/
```
