# Shiden — Formatting

Shiden code uses consistent spacing and line breaks to stay readable.

## Indentation

Use 4 spaces per level and indent all statements within a block.
Block closing markers like `fn/`, `if/`, `while/`, `for/` are placed at the same level as their opening keyword.

```shiden
fn new main/
    let x = 0/i64
    while x < 3/
        x = x + 1/i64
    while/
fn/
```

## Spacing around operators

Binary operators are surrounded by single spaces.
Unary operators are written directly before their operand with no space.

```shiden
let a = 1 + 2/i64
let b = -5/i64
let ok = x == 3 && y != 0/bool
```

## Function calls and argument lists

No space between the function name and the opening parenthesis.
Arguments are separated by commas followed by a single space.

```shiden
println("hello")/unit
add(1, 2)/i64
```

## Line breaks and statements

Each statement ends with `/` or `/<type>` and is on its own line.
Long lines can be split before operators or between arguments.

```shiden
let result = very_long_function_name(
    arg1, arg2, arg3
)/i64
```

## File structure

Imports are grouped at the top and separated from functions by a blank line.
Each function is separated by a blank line for clarity.
However if you do not do this it will still compile.

```shiden
let "fs" = fs/import

fn new helper/
    println("hi")/unit
fn/

fn new main/
    helper()/unit
fn/
```

## Comments

Shiden does not yet have comment syntax.
For now, use string literals or separate documentation files for notes.
A plan for commenting is in the works and will be added in a future update.
