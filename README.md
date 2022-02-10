
# LichenScript

A ligtweight language compiled to JavaScript/C.
Easy to integrated with the Web Ecosystem.

## Why

LichenScript is designed to write untrustworthy plugins for a exist system.
For example, LichenScript can be used to write third-party plugins for your website/mobile app, which is isolated.
It has modern syntaxes such as pattern matching/discriminated union.
It's designed to be lightweight, easy to compile to target platform with very small overhead.

# Feature

- Modern syntax, close to TypeScript/JavaScript
- Static typing
- Running on almost all platforms through C/JavaScript
- Isolate environment
- Very lightweight runtime and very small overhead

## Senerios

- Plugin system for a Website
- Game development
- Mini-program environment

# Install

```
npm install -g lichenscript
```

# Roadmap

✅ Done
🔨 Work in progress
📖 Planning

| Syntax | Status |
| ------ | ------ |
| i32/f32 | ✅ |
| String | ✅ |
| Array/Map | ✅ |
| Class/Enum | ✅ |
| Discriminated union | ✅ |
| Lambda Expression | ✅ |
| Pattern matching | ✅ |
| Object inheritance | ✅ |
| i64/f64 | 🔨 |
| Tuple | 🔨 |
| For Iterator | 🔨 |
| Customize getter/setter | 🔨 |
| Operator overloading | 📖 |
| Async/await | 📖 |

| Stdlib | Status |
| ------ | ------ |
| Array | ✅ |
| Map | ✅ |
| Regex | 📖 |

| Target | Status |
| ------ | ------ |
| Compiled to C (macOS/Linux) | ✅ |
| Compiled to C (Emscripten/WASM) | ✅ |
| Compiled to JavaScript | 🔨 |

# FAQ

## What's the different with Rust? Why not Rust?

LichenScript does NOT use lifetime.

LichenScript is a high level language.
I don't want the programmers to care about the detail
of the memory such as liftime/smart pointer.

LichenScript is designed to use the ecosystem of C++ in
Computer Graphic. So compiling to C is reasonable.

It can be easily integrated with Rust with C-ABI.

### LichenScript is more dynamic than Rust

- Doesn't have a bunch of pointer types.
- Support inheritance.

## What's different from JavaScript/TypeScript?

- More lightweight to run, don't depends on a heavy runtime.
- The C runtime is very small, LichenScript's object uses reference counting to free memory.
- More static features.

### No exceptions and try/catch

Exception mechanism depends on a heavy runtime.
So it's exclude from the language.
