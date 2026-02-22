# Shiden — Types

Types are postfix tokens written as `/<type>`.

For working examples of all types, see `examples/docs/src/main.sd`.

## Primitive types

Built-in types: `u8`, `u16`, `u32`, `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `str`, `char`, `bool`, `array`, `unit`, `import`.
Type names are always lowercase and only appear as postfix suffixes.

```shiden
let count = 42/u64
let name = "Kai"/str
let ok = true/bool
```

## Numeric literals

Integers and floats are parsed from digits; the type comes from the suffix.
Float literals require a decimal point and a float type suffix.

```shiden
let a = 10/i64
let b = 3.14/f64
```

## Strings and chars

Strings use double quotes and support escapes like `\n`, `\t`, `\"`, `\\`.
Chars use single quotes and represent a single Unicode scalar.

```shiden
let s = "line1\nline2"/str
let c = 'x'/char
```

## Bool

Boolean literals are `true` and `false`.
Use them with `/bool` when binding to a variable.

```shiden
let ready = false/bool
```

## Arrays

Array literals use brackets and commas and are typed with `/array`.
Array elements are stored as boxed values at runtime.

```shiden
let nums = [1, 2, 3]/array
```

## Unit

`unit` represents no value and is used for effect-only calls.
Functions that return nothing usually end with `/unit`.

```shiden
println("ok")/unit
```
