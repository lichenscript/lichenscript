
# LichenScript

A ligtweight language compiled to JavaScript/C.
Easy to integrated with the Web Ecosystem.

## Why

LichenScript is designed to write untrustworthy plugins for a exist system.
For example, LichenScript can be used to write third-party plugins for your website/mobile app, which is isolated.
It has modern syntax such as pattern matching/discriminated union.
It's designed to be lightweight, easy to compile to target platform with very small overhead.

# Feature

- Modern syntax, close to TypeScript/JavaScript
- Static typing
- Running on almost all platform through C/JavaScript
- Isolate environment
- Very lightweight runtime and very small overhead

## Senerios

- Web game development
- Sanbox environment in Web
- Plugin engine for your Web App
- Mini-program environment

# Roadmap

✅ Done
🔨 Work in progress
📖 Planning

| Syntax | Status |
| ------- | ------ |
| Lambda Expression | ✅ |
| Pattern matching | ✅ |
| Object inheritance | ✅ |
| Tuple | 🔨 |
| For Iterator | 🔨 |
| Customize getter/setter | 🔨 |
| Operator overloading | 📖 |
| Async/await | 📖 |

| Stdlib | Status |
| Array | ✅ |
| Map | ✅ |

| Target | Status |
| ------ | ------ |
| Compiled to C (macOS/Linux) | ✅ |
| Compiled to C (Emscripten/WASM) | ✅ |
| Compiled to JavaScript | 🔨 |

# FAQ

## What's the different with Rust? Why not Rust?

### LS do NOT use lifetime

LS is high level language.
I don't want the programmer to care about the detail
of the memory such as liftime/smart pointer.

LS is designed to use the ecosystem of C++ in
Computer Graphic. So comping to C is reasonable.

It't not hard to support Rust in the future.

### LS is more dynamic than Rust

No a bunch of pointer types.
Dynamic dispatch, supports any.
Support inheritance.

## What's different from JavaScript/TypeScript?

More lightweight, don't depends on a heavy runtime.
The C runtime is very small, LS's object uses reference
counting to free memory.

More static features.

### class private field are guaranteed by the type system

Unlike JavaScript/TypeScipt, which is 'fake' private,
LS's private is guaranteed, so you don't need a `#` syntax
to implement real private field.

### No exceptions and try/catch

Exception mechanism depends on a heavy runtime.

### More static
