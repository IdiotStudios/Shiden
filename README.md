# Shiden

![License](https://img.shields.io/github/license/IdiotStudios/Shiden)
![Stars](https://img.shields.io/github/stars/IdiotStudios/Shiden)
![Issues](https://img.shields.io/github/issues/IdiotStudios/Shiden)

Shiden is a high-performance ahead-of-time compiled language focused on speed, predictability, and low-level control, with a syntax familiar to Rust and JavaScript.

## NOTE:
Shiden is in early development and is not yet stable. Expect breaking changes and missing features. Contributions and feedback are welcome! Also I am in the process of rewriting teh rust implementation in ASM so expect some instability and missing features for a while. Release [v0.1.2](https://github.com/IdiotStudios/Shiden/releases/tag/v0.1.2) is the stable rust release, but the asm release is still in progress.

## LowShiden

LowShiden is a minimal universal low-level language for Shiden.

The goal of LowShiden is to provide a simple, readable, cross-platform assembly-like language that can:
* bootstrap the Shiden compiler
* allow Shiden to become self-hosting
* provide a universal backend target
* remain small enough to understand fully

### Design Goals

* Small instruction set (~40 instructions)
* Readable syntax
* Universal across CPU architectures
* Easy compiler backend mapping
* Capable of self-hosting

### Registers

LowShiden provides 16 general purpose registers.

```
r0
r1
r2
r3
r4
r5
r6
r7
r8
r9
r10
r11
r12
r13
r14
r15
```

Libraries can create readable register aliases.

Example:

```
alias r(result) = r0
alias r(tmp) = r1
alias r(arg0) = r2
alias r(arg1) = r3
```

Usage:

```
add r(arg0) r(arg1)
mov r(result) r(arg0)
```

### Instruction Set (~40 Instructions)

#### Data Movement

```
mov dst src
load dst addr
store addr src
push src
pop dst
swap a b
```

#### Arithmetic

```
add a b
sub a b
mul a b
div a b
mod a b
inc a
dec a
neg a
```

#### Bit Operations

```
and a b
or a b
xor a b
not a
shl a b
shr a b
```

#### Comparison

```
cmp a b
test a
```

#### Control Flow

```
jmp label
je label
jne label
jg label
jl label
jge label
jle label
```

#### Function Control

```
call label
ret
enter
leave
```

#### Memory / Addressing

```
lea dst addr
alloc size
free ptr
```

#### System Interaction

```
syscall
trap
halt
```

### Example: Function

```
alias r(result) = r0
alias r(a) = r1
alias r(b) = r2

add r(a) r(b)
mov r(result) r(a)
ret
```

### Example: Linux Socket (Conceptual)

```
mov r0 41
mov r1 2
mov r2 1
mov r3 0
syscall
```

### Bootstrapping Plan

1. Implement LowShiden compiler
2. Write core runtime in LowShiden
3. Implement Shiden compiler using LowShiden
4. Recompile Shiden using itself

At this stage Shiden becomes self-hosting.

## Features

- Ahead-of-time compilation
- Familiar Rust/JavaScript-like syntax
- Built-in low-level system primitives
- Built-in libraries implemented as optimized x86-64 machine code
- Fast startup and predictable performance

## Why Shiden?

Shiden is designed for developers who want the performance of systems languages without the complexity that often comes with them.

It focuses on:

- predictable performance
- simple syntax
- direct access to system primitives
- minimal runtime overhead

### Goal
Shiden aims to combine the performance and control of Rust with the simplicity of scripting languages while exposing low-level system primitives directly.

### Platforms

| Platform | Arch | Support | Phase |
| :--- | :---: | :---: | ---: |
| `linux` | x86_64 | No | N/A |
| `linux` | arm | No | N/A |
| `windows` | x86_64 | No | N/A |
| `macos` | N/A | No | N/A |
More architectures and platforms are planned.

### Random Thought
GitHub should add polls to repos. That would be peak.

## Installation

### Going to be added soon

## Ecosystem

- 🧠 **[VSCode-Extension](https://marketplace.visualstudio.com/items?itemName=IdiotStudios.shiden)**  
- 🍽️ **[Flavortown-Project](https://flavortown.hackclub.com/projects/10866)**  
- 📚 **[Shiden-Docs](https://docs.shiden.kirze.de/)**  
- 📦 **[Shiden-Package-Registry]()** (hint...)  
- 🧪 **[Benchmarks]()** (hmm very soon)  
(thanks kde emoji picker)

## Cli - Rust version
| Command | Description | Status |
| :--- | :--- | :---: |
| `shiden run` | run shiden projects | ✅ |
| `shiden check` | runs checks | ✅ |
| `shiden new {name}` | creates a new project | ✅ |
| `shiden compile` | compiles Shiden (run does the same anyway) | ✅ |
| `shiden help` | shows help | ✅ |

## CLI - Assembly version
| Command | Description | Status |
| :--- | :--- | :---: |
| `shiden run` | release mode by default, add `--debug` for debug mode | ✅ |
| `shiden check` | runs checks | ✅ |
| `shiden new {name}` | creates a new project | ✅ |
| `shiden init` | initializes project in current directory | ✅ |
| `shiden compile` | compiles Shiden (run does the same anyway) | ✅ |
| `shiden help` | shows help | ✅ |

### Shiden init/new folder formats

```
my_project/  
├── shiden.toml         // Manifest  
├── src/  
│   └── main.sd         // Executable entry  
├── tests/  
│   └── example.sd  // doesn't work yet  
├── build/              // Compiled binaries  
│   ├── cache/   // Build cache (none)  
│   ├── linux/   // Linux target  
│   ├── windows/ // Windows target  
│   └── macos/   // macOS target  
└── README.md  
```

## Supporters

Shiden is a project supported by [GitBook](https://www.gitbook.com/?utm_source=content&utm_medium=sponsored-by-gitbook&utm_campaign=xaAHE1MNwN8fiIAs9Mux)! If you want to support the project then consider sponsoring or donating! It really helps a lot and is very much appreciated.

## Contributing

If you do wanna contribute then feel free to create issues or pull requests! Love the support.
Also Checkout the [CONTRIBUTING.md](CONTRIBUTING.md) for more details on how to contribute.

## License

Licensed under the MIT License. See the `LICENSE` file for details.

## AI Notice

Used AI for inline code completions and for documentation writing. Also for debugging.