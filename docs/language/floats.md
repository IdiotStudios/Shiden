# Shiden — Floating-Point Support

Floating-point numbers provide high-precision decimal computation using IEEE 754 format. Shiden supports both 32-bit (f32) and 64-bit (f64) floats, as well as generic bit-width float types (fX).

## Float literals

Float literals contain a decimal point and must be typed with a float type suffix.

```shiden
let x = 3.14/f64
let y = 2.5/f32
let pi = 3.14159265359/f64
let epsilon = 0.0001/f64
```

## Float arithmetic

All four basic arithmetic operations are supported for floats:

```shiden
let a = 1.5/f64
let b = 2.5/f64

println("Add: {}", 1.5 + 2.5)/       # 4.0
println("Sub: {}", 10.0 - 3.5)/      # 6.5
println("Mul: {}", 2.0 * 3.5)/       # 7.0
println("Div: {}", 15.0 / 3.0)/      # 5.0
```

## Constant folding

When both operands in a float arithmetic expression are literals (constants), the operation is evaluated at compile-time:

```shiden
println("Result: {}", 2.5 + 3.14)/   # Evaluates to 5.64... at compile time
println("Result: {}", 10.0 / 4.0)/   # Evaluates to 2.5 at compile time
```

This optimization reduces runtime overhead for fixed calculations.

## IEEE 754 semantics

Float operations follow IEEE 754 standard semantics, including:

### Precision artifacts
Due to binary representation, some decimal values don't have exact binary representations:

```shiden
println("{}", 0.1 + 0.2)/   # prints 0.30000000000000004 (not exactly 0.3)
```

This is expected behavior in all languages implementing IEEE 754.

### Rounding
Operations are rounded using "round-to-nearest, ties-to-even" (banker's rounding), the IEEE 754 default.

## Available float types

### Fixed-width types
- **f32**: 32-bit IEEE 754 single precision (7 decimal digits)
- **f64**: 64-bit IEEE 754 double precision (15-17 decimal digits)

```shiden
let single = 3.14/f32    # 7 significant digits
let double = 3.14/f64    # 15-17 significant digits
```

### Generic bit-width types (fX)
Shiden recognizes any pattern `fX` where X is a bit width, though floating-point types are inherently signed per IEEE 754.

```shiden
let f32_val = 1.5/f32
let f64_val = 1.5/f64
let f128_val = 1.5/f128  # Recognized; no special 128-bit support yet
```

## SSE implementation

Float operations use SSE (Streaming SIMD Extensions) instructions on x86_64:
- **ADDSD**: Add scalar double (f64)
- **SUBSD**: Subtract scalar double
- **MULSD**: Multiply scalar double
- **DIVSD**: Divide scalar double
- **COMISD**: Compare scalar double

This provides native hardware acceleration for floating-point operations.

## Limitations and future work

### Current limitations
1. **Float variables**: Variables with float-typed assignments don't print correctly (prints as bit pattern instead of decimal)
   ```shiden
   let x = 1.5/f64
   println("{}", x)/   # prints bit representation, not 1.5
   ```
   
2. **Mixed operations**: Integer + float operations not yet fully supported
   ```shiden
   println("{}", 5 + 2.5)/  # Currently would not work correctly
   ```

3. **Float comparisons**: Comparison operators work but may need careful use
   ```shiden
   let a = 1.0/f64
   let b = 1.0/f64
   println("{}", a == b)/   # Should work, but tested less thoroughly
   ```

### Future enhancements
- Automatic type coercion between int and float
- Float-to-string pretty-printing
- Math library functions (sqrt, sin, cos, log, etc.)
- Float parsing from strings
- Extended precision (f128, f256) on supported platforms

## Type safety

Unlike some languages, Shiden requires explicit float types on all float literals and variables:

```shiden
let x = 3.14/f64   # Correct: type explicitly specified
let y = 3.14       # Error: float literal requires type suffix
```

This ensures clarity and prevents accidental type conversions.

## Examples

### Scientific calculation
```shiden
fn new main/
    let pi = 3.14159265359/f64
    let radius = 5.0/f64
    let area = pi * radius * radius/f64
    println("Circle area: {}", area)/
fn/
```

### Numerical precision
```shiden
fn new main/
    println("IEEE 754 precision:")/
    println("0.1 + 0.2 = {}", 0.1 + 0.2)/
    println("sqrt(2) ≈ {}", 1.4142135623730951)/
fn/
```
