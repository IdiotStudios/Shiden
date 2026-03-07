# Shiden — Expressions

Expressions are used in lets, assignments, and calls.

## Operators and precedence

Order is: unary (`-`, `!`), `*` and `/`, `+` and `-`, comparisons, `&&`, `||`.
All operators are left-associative at the same precedence level.

```shiden
let ok = (1 + 2 * 3) == 7/bool
```

## Calls and indexing

Calls use parentheses and indexing uses brackets.
Indexing can be applied to variables or call results.

```shiden
let nums = [4, 5, 6]/array
println("{}", nums[1])/
```

## Grouping and unary

Parentheses group expressions, and unary ops apply to the right.
Unary `!` is only defined on boolean values.

```shiden
let v = -(1 + 2)/i64
```
