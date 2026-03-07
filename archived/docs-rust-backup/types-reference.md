# Shiden — Complete Type Reference

Quick reference for all supported types in Shiden.

## Type summary

| Category | Types | Count |
|----------|-------|-------|
| Fixed-width unsigned integers | u8, u16, u32, u64, usize | 5 |
| Fixed-width signed integers | i8, i16, i32, i64 | 4 |
| Fixed-width floats | f32, f64 | 2 |
| Generic signed integers | i1 through i63 | 63 |
| Generic unsigned integers | u1 through u63 | 63 |
| Generic floats | fX (any X from 1-128) | Unlimited |
| String/Character | str, char | 2 |
| Container | array | 1 |
| Special | bool, unit | 2 |
| Module system | import | 1 |

**Total: 143+ type variants**

---

## Integer types

### Fixed-width signed integers (4 types)

```
i8:   -128 to 127
i16:  -32,768 to 32,767
i32:  -2,147,483,648 to 2,147,483,647
i64:  -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
```

### Fixed-width unsigned integers (5 types)

```
u8:    0 to 255
u16:   0 to 65,535
u32:   0 to 4,294,967,295
u64:   0 to 18,446,744,073,709,551,615
usize: Platform-dependent (typically u64 on 64-bit systems)
```

### Generic bit-width signed integers (63 types: i1 to i63)

```
i1:    -1 to 0                      (1 bit)
i2:    -2 to 1                      (2 bits)
i3:    -4 to 3                      (3 bits)
i4:    -8 to 7                      (4 bits)
i5:    -16 to 15                    (5 bits)
...
i63:   -4,611,686,018,427,387,904 to 4,611,686,018,427,387,903
```

Auto-truncation example:
```
255/i4  →  15  (255 & 0xF)
16/i4   →  0   (16 & 0xF)
```

### Generic bit-width unsigned integers (63 types: u1 to u63)

```
u1:    0 to 1                     (1 bit)
u2:    0 to 3                     (2 bits)
u3:    0 to 7                     (3 bits)
u4:    0 to 15                    (4 bits)
u5:    0 to 31                    (5 bits)
...
u63:   0 to 9,223,372,036,854,775,807
```

Auto-truncation example:
```
256/u8  →  0   (256 & 0xFF)
3/u1    →  1   (3 & 0x1)
```

---

## Floating-point types

### Fixed-width floats (2 types)

```
f32:  32-bit IEEE 754 single precision  (7 significant digits)
f64:  64-bit IEEE 754 double precision  (15-17 significant digits)
```

### Generic floating-point types (fX pattern)

Recognized: `f32`, `f64`, and any pattern `fX` where X is 1-128

```
f1, f2, f3, ..., f32, ..., f64, ..., f128
```

Note: All floats are inherently signed per IEEE 754 standard.

---

## Text and characters

```
str:   UTF-8 string literal (immutable)
char:  Single Unicode scalar value
```

---

## Container types

```
array: Dynamic array/vector of values
```

---

## Boolean and void

```
bool:  true or false
unit:  No value (void) - can write / or /unit
```

---

## Module system

```
import: Module import identifier
```

---

## Usage examples

```shiden
# Signed integers
let small = 7/i4
let large = 1000000/i64

# Unsigned integers
let bits = 255/u8
let exact = 100/u16

# Floats
let pi = 3.14159/f64
let e = 2.71828/f32

# Text
let msg = "hello"/str
let initial = 'A'/char

# Container
let data = [1, 2, 3]/array

# Boolean and void
let ready = true/bool
println("done")/
```

---

## Type constraints

- **Explicit typing required**: All numeric literals must have explicit type suffix
- **No implicit conversion**: Assigning between different numeric types requires explicit operations
- **Bit-width enforcement**: Values automatically truncate to fit their type width
- **IEEE 754 compliance**: Floats follow standard precision and rounding rules

---

## Special properties

### Truncation (integer types)
Values wider than the target type are masked to fit:
- `255/i4` → 15 (4-bit mask: 255 & 0xF = 15)
- `256/u8` → 0 (8-bit mask: 256 & 0xFF = 0)

### Precision (float types)
Float to float binary is exact per IEEE 754:
- `0.1 + 0.2` may give `0.30000000000000004` due to binary representation
- This is expected behavior across all IEEE 754 implementations

### Constant folding
Float literal arithmetic is evaluated at compile-time:
- `1.5 + 2.5` evaluates to `4.0` before runtime

### SSE optimization
Float operations use SSE instructions (x86_64) for hardware acceleration:
- ADDSD, SUBSD, MULSD, DIVSD, COMISD

---

## Type signatures in functions

```shiden
fn add = (x/i32, y/i32) i32
fn divide = (a/f64, b/f64) f64
fn make_string = () str
fn do_nothing = () /
```

---

## Migration guide

### From fixed types to generic types
```shiden
# Old (still works)
let x = 100/i64

# New (more flexible)
let y = 100/i32    # Smaller storage
let z = 100/i20    # Custom precision
```

### From integers to floats
```shiden
# Integers
let count = 42/i64

# Floats
let ratio = 3.5/f64

# Note: explicit conversion needed for i+f
let sum = 5 + 2.5  # Currently unsupported, use 5.0/f64
```
