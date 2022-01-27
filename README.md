
# LichenScript

A ligtweight language compiled to JavaScript/C.
Easy to integrated with the Web Ecosystem.

## Why

Webassembly(AKA WASM) provides excellent performance approching native speed, but it's hard for front-end ecosystem to use it.

## Roadmap

| Syntax | Status |
| ------- | ------ |
| Lambda Expression | ✅ |
| Pattern matching | ✅ |
| Object inheritance | ✅ |
| For Iterator | 🔨 |
| Operator overloading | 🔨 |
| Customize getter/setter | 🔨 |
| Async/await | 🔨 |

| Target | Status |
| ------ | ------ |
| Compiled to C | ✅ |
| Compiled to JavaScript | ✅ |

## Senerios

- Web game development
- Sanbox environment in Web
- Plugin engine for your Web App
- Mini-program environment

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

# License

Two cases:

1. The compiler of LichenScript(written in OCaml)
   is under GPLv3 license.
2. The standard library(written in LichenScript)
   is under MIT license.
