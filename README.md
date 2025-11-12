# Ray

**Ray** is a statically typed, interpreted language that aims for the sweet spot between **safety** and **efficiency**.  
It focuses on **clarity**, **robustness**, and **runtime performance**, while keeping syntax expressive and lightweight.

Ray is implemented in **Zig** and runs on its own **stack-based virtual machine** with a **mark-and-sweep garbage collector**.

![Pipeline Tests](https://github.com/martazvch/ray/actions/workflows/main.yml/badge.svg)

## Why Ray

Ray brings together ideas from the languages I admire most.  
It’s heavily inspired by **Rust** and **Zig** syntaxes, yet remains **interpreted**, making it easy to use, easy to embed, and simple to learn.

While its primary goal is to serve as an **embeddable scripting language**, Ray is general-purpose and flexible enough for standalone programs — especially thanks to its **native C and Zig interop**.

## Key features

- Statically typed — combines strong typing with type inference for fast iteration
- Expression oriented — every block and control flow construct can return a value
- Error unions — built-in error handling via `T!ErrorType`
- Nullable types with `?` — explicit handling of optional values and fallback values
- Traits and structures — composition-based design with methods and generic behaviors
- Pattern matching — `when` expressions for type based pattern matching and `match` for value matching
- Garbage-collected memory — efficient mark-and-sweep collector integrated with the VM
- Macros — metaprogramming on the AST without preprocessors
- Interop with Zig and C — seamless integration for embedding or extending native code
- Simple, consistent syntax — designed to be readable like a scripting language, yet safe like a systems language

---

## Documentation

A work-in-progress specification describes the current syntax, semantics, and design goals of the language.

- [Language Specification](docs/spec.md)

---

## Build from source

To build Ray you need [Zig](https://ziglang.org/) compiler version `0.15.2`.
Clone the repo and run:

```sh
git clone https://github.com/martazvch/ray.git
cd ray
zig build -Doptimize=ReleaseFast
```

---

## Tests

I try to cover the maximum errors/use case as possible by testing each individual stage of the pipeline: Ast generation, Analyzer IR output, compiled bytecode and then VM's runtime behavior.
You can find them in the [tests](tests/) folder.

To run them, run:
```sh
zig build test -Dtest-mode -Dstress-gc
```
