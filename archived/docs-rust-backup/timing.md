# Shiden — High-Resolution Timing

Shiden reports compilation and execution times with nanosecond precision using platform-specific high-resolution timers.

## Output Format

Timing is displayed in the format: `XmYsZmsAnsXps` where only non zero values are shown.

For example:
- `6ms 554ns` — 6 milliseconds and 554 nanoseconds
- `623ns` — 623 nanoseconds
- `1m 30s 45ms` — 1 minute, 30 seconds, and 45 milliseconds

If the time rounds to zero, `0ns` is displayed.

## Platform-Specific Implementation

### Windows
Uses **QueryPerformanceCounter** and **QueryPerformanceFrequency** from the Windows API for sub-microsecond precision. Can capture picosecond-level intervals on high-frequency performance counters.

### macOS
Uses **mach_absolute_time()** and **mach_timebase_info()** from the Mach kernel API. Provides nanosecond precision with potential for higher resolution on supported hardware.

### Linux
Uses **clock_gettime(CLOCK_MONOTONIC_RAW)** for monotonic timing independent of system clock adjustments. Provides nanosecond precision.

## Timing Messages

**Compiled in**: Time taken to compile Shiden source code to a native executable (x86_64-linux, x86_64-windows, or x86_64-macos).

**Finished in**: Time taken to run the compiled executable.

Both messages appear when running a project or single source file with `shiden run`.

## Precision Limits

- **Nanoseconds (ns)**: Supported on all platforms
- **Picoseconds (ps)**: Displayed only if captured by the platform timer; typically requires specialized hardware
- **Sub-nanosecond**: Not supported by standard system timers

Example output:
```
Compiled in 5ms 217ns
...program output...
Finished in 1s 234ms 567ns
```
