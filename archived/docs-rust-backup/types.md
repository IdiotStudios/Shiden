# Shiden — Types

Types are postfix tokens written as `/<type>`.
They are required on `let` statements and in function signatures.

For working examples of all 267+ types, see `examples/types/src/main.sd`.

## Primitive types

Built-in fixed-width types: `u8`, `u16`, `u32`, `u64`, `usize`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `str`, `char`, `bool`, `array`, `unit`, `import`.

Shiden also supports **128 signed integer types** (i1-i128), **128 unsigned integer types** (u1-u128), and **5 floating-point types** (f16, f32, f64, f80, f128).

Type names are always lowercase and only appear as postfix suffixes.

```shiden
let count = 42/u64
let name = "Kai"/str
let ok = true/bool
let pi = 3.14/f64
```

## Generic bit-width numeric types

In addition to fixed-width types, Shiden supports **generic bit-width types** with any bit width from 1 to 128 bits.

### Signed integers (iX pattern)
Full support for all signed integer bit-widths from 1 to 128 bits: `i1`, `i2`, `i3`, ..., `i127`, `i128` (128 total variants)

```shiden
let a = 15/i4       # 4-bit signed: max value = 7, truncates as 15 & 0xF = 15
let b = 255/i8      # 8-bit signed: max value = 127
let c = 65535/i16   # 16-bit signed: max value = 32767
let d = 4294967295/i32  # 32-bit signed: max value = 2147483647
let e = 100/i100    # 100-bit signed integer
let f = 200/i127    # 127-bit signed integer
let g = 255/i128    # 128-bit signed integer
```

Values are automatically truncated to fit the bit width using AND masking. For example, `255/i4` stores the value `15` (since 255 in binary is `11111111`, masked to 4 bits gives `1111` = 15).

### Unsigned integers (uX pattern)
Full support for all unsigned integer bit-widths from 1 to 128 bits: `u1`, `u2`, `u3`, ..., `u127`, `u128` (128 total variants)

```shiden
let a = 3/u1        # 1-bit unsigned: max value = 1, so 3 & 0x1 = 1
let b = 255/u8      # 8-bit unsigned: max value = 255
let c = 65535/u16   # 16-bit unsigned: max value = 65535
let d = 4294967295/u32  # 32-bit unsigned: max value = 4294967295
let e = 100/u100    # 100-bit unsigned integer
let f = 200/u127    # 127-bit unsigned integer
let g = 255/u128    # 128-bit unsigned integer
```

### 128-bit integers (i128, u128)
Shiden supports full `i128` and `u128` types for extended integer ranges:

- `i128`: 128-bit signed integers (-170141183460469231731687303715884105728 to 170141183460469231731687303715884105727)
- `u128`: 128-bit unsigned integers (0 to 340282366920938463463374607431768211455)

```shiden
let big_int = 100000000000000000000/i128
let big_uint = 100000000000000000000/u128
let result = big_int + big_int/i128  # arithmetic works within 64-bit range
```

**Note:** While i128 and u128 types are fully declared and supported, arithmetic operations are currently computed with 64-bit precision. For values within the i64/u64 range, operations are accurate. Values exceeding 64-bit range may overflow.

### Floating-point types (fX pattern)
Supported floating-point types: `f16`, `f32`, `f64`, `f80`, `f128` (5 standard types), and any pattern `fX` where X is a bit width (1-128).

```shiden
let pi = 3.14159/f64
let e = 2.71828/f32
let approx = 1.414/f16
let extended = 3.14159265358979/f80
let quad = 3.14159265358979323846/f128
```

**Note:** Floating-point types are inherently signed (following IEEE 754). There are no unsigned float variants. Float operations use SSE instructions on x86_64 for accurate arithmetic. Float variables display as bit patterns (internal 64-bit representation) rather than decimal values.

## Numeric literals

Integers and floats are parsed from digits; the type comes from the suffix.
Integer literals are whole numbers without a decimal point.
Float literals must have a decimal point.

```shiden
let int_val = 10/i64        # integer literal
let float_val = 3.14/f64    # float literal with decimal point
let truncated = 255/i4      # 255 truncated to 4 bits = 15
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
You can also just do `/` instead of `/unit`.

```shiden
println("ok")/
```

## Complete type reference

### Fixed-width integer types
| Type | Bits | Signed | Range |
|------|------|--------|-------|
| `i8` | 8 | Yes | -128 to 127 |
| `i16` | 16 | Yes | -32768 to 32767 |
| `i32` | 32 | Yes | -2147483648 to 2147483647 |
| `i64` | 64 | Yes | -9223372036854775808 to 9223372036854775807 |
| `u8` | 8 | No | 0 to 255 |
| `u16` | 16 | No | 0 to 65535 |
| `u32` | 32 | No | 0 to 4294967295 |
| `u64` | 64 | No | 0 to 18446744073709551615 |
| `usize` | Platform-dependent | No | Platform-dependent |

### Generic bit-width integer types
| Pattern | Bits | Signed | Example |
|---------|------|--------|---------|
| `iX` | 1-128 | Yes | `i4`, `i24`, `i100`, `i128` |
| `uX` | 1-128 | No | `u4`, `u24`, `u100`, `u128` |

Supported generic signed types: `i1`, `i2`, `i3`, ..., `i127`, `i128` (128 total variants)
Supported generic unsigned types: `u1`, `u2`, `u3`, ..., `u127`, `u128` (128 total variants)

**Truncation behavior:** Values are automatically masked to fit the specified bit width. For example:
- `256/u8` → `0` (256 & 0xFF)
- `255/i4` → `15` (255 & 0xF)
- `65536/u16` → `0` (65536 & 0xFFFF)

### Floating-point types
| Type | Bits | Format | Special Notes |
|------|------|--------|---------------|
| `f16` | 16 | IEEE 754 Half | Half-precision float |
| `f32` | 32 | IEEE 754 Single | Single-precision float |
| `f64` | 64 | IEEE 754 Double | Default/double-precision float |
| `f80` | 80 | Extended | Extended-precision float |
| `f128` | 128 | Quadruple | Quadruple-precision float |
| `fX` | 1-128 | Generic | Any bit-width pattern recognized |

All 5 standard float types (f16, f32, f64, f80, f128) are fully supported. Float operations using SSE instructions provide accurate arithmetic.

**Examples:**
```shiden
let pi = 3.14159/f64
let e = 2.71828/f32
let sum = 1.5 + 2.5  # evaluates to 4.0 at compile time
```

### String and character types
| Type | Purpose |
|------|---------|
| `str` | Fixed-length string (immutable) |
| `char` | Single Unicode scalar value |

### Container types
| Type | Purpose |
|------|---------|
| `array` | Dynamic array/vector of values |

### Special types
| Type | Purpose |
|------|---------|
| `bool` | Boolean: `true` or `false` |
| `unit` | No value (void/nil) |

## Type conversions and mixing

- **Integer types**: Implicit truncation applies when assigning to narrower types
- **Float literals**: Always expressible; operations between floats use SSE
- **Mixed int+float**: Currently requires explicit handling (future enhancement)
- **String/char**: Immutable; chars convert to/from integers via UTF-8

## Type inference limitations

Shiden requires explicit type annotations on all `let` bindings and function parameters/return types.
There is no automatic type inference between numeric types.

```shiden
let x = 42/i64       # must specify /i64
let y = 10/u8        # must specify /u8
let z = 3.14/f64     # must specify /f64
```

