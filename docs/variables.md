# Shiden — Variables

Variables are declared with `let` and optionally `mut`.

## let and let mut

`let` requires a type suffix on the statement.
Use `mut` only when you plan to reassign the binding.

```shiden
let mut total = 0/i64
```

## Assignment

Assignments end with `/`.
Use `let` to introduce a type, then reassign without a suffix.

```shiden
total = total + 1/
```

## Index assignment

Use brackets to assign into arrays.
Indices are zero-based and must be integer expressions.

```shiden
let nums = [1, 2, 3]/array
nums[1] = 9/
```
