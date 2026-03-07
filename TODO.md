# Todo

## Compiler parity (linux asm)

- [x] Array index literals resolve correctly (`arr[0]`, `arr[1]`, `arr[2]`)
- [x] Array index assignment works (`arr[i] = value`)
- [x] `if / else` execution in docs example
- [x] `while` execution in docs example
- [x] `for` execution in docs example
- [x] `break` and `continue` behavior in loops
- [x] Function returns in docs example (`return 42`, `return a + b`)
- [x] Dynamic array element lookup after `push` (`dynamic[1]`)
- [x] `len(dynamic)` as a direct `println` argument in formatter path

## libraries

- [ ] `Graphics` - 2D graphics library with software rendering backend
- [ ] `Audio` - audio playback library with support for multiple backends
- [ ] `Inputs` - inputs library for handling keyboard, mouse, and gamepad input
- [ ] `Networking` - high-level networking library for TCP/UDP communication
- [ ] `Filesystem` - library for file I/O and path manipulation
- [ ] `Process` - library for spawning and managing subprocesses
- [ ] `Time` - library for working with time and timers
- [ ] `Math` - library for common math functions and data structures (vectors, matrices, etc.)
- [ ] `Concurrency` - library for working with threads and async tasks
- [ ] `Database` - library for working with databases (SQLite, Postgres, etc)
- [ ] `Graphics 3d` - 3D graphics library with software rendering backend
- [ ] `Audio 3d` - 3D audio library with support for multiple backends
- [ ] `Web` - library for making HTTP requests and working with web APIs
- [ ] `GUI` - library for building graphical user interfaces

## Targets

- [ ] `linux-x86_64`
- [ ] `linux-arm`
- [ ] `windows-x86_64`
- [ ] `macos-x86_64`
- [ ] `macos-arm64`
- [ ] `wasm32`
- [ ] `android`
- [ ] `ios`
- [ ] `iso`
- [ ] `bootloader`
- [ ] `driver/kernel`

## Tooling

- [ ] `Package Manager` - tool for managing and publishing packages
- [ ] `Formatter` - code formatter for Shiden source files
- [x] `Linter` - tool for analyzing code for potential errors and style issues
- [ ] `Language Server` - implementation of the Language Server Protocol for Shiden
- [ ] `Debugger` - tool for debugging Shiden programs
- [ ] `Profiler` - tool for profiling the performance of Shiden programs
- [ ] `Build System` - tool for building and managing Shiden projects
- [ ] `Documentation Generator` - tool for generating documentation from Shiden source code

## CLI

- [x] `shiden run` - Compile and run (--debug for debug mode)
- [x] `shiden check` - Run checks
- [ ] `shiden new {name}` - Create a new project
- [ ] `shiden init` - Initialize a project in the current directory
- [x] `shiden compile` - Compile Shiden (same as run but without running)
- [x] `shiden help` - Show help message
- [ ] `shiden update` - Check/install updates

## Misc

- [ ] `Benchmarks` - comprehensive benchmarks comparing Shiden to other languages
- [x] `Examples` - collection of example projects demonstrating various features of Shiden