# Tao

Tao is a statically-typed functional programming language.

```
data Maybe A =
    | Just A
    | None

def nth A : Nat -> [A] -> Maybe A = fn
    | 0, [x ..] => Just x
    | n, [_ .. tail] => tail:nth(n - 1)
    | _, _ => None
```

## Features

- Type inference
- Useful error messages
- First-class functions
- Currying
- Sum types
- Records
- Polymorphism through type parameters
- Pattern matching
- Built-in lists
- MIR optimiser
- Bytecode compiler

## Planned features

- Better syntax
- Typeclasses (or ML-style generic modules?)
- Module system
- Monadic IO (or an effect system?)
- Do notation
- LLVM backend

## Why?

Tao is primarily a personal hobby project. I have no real aspirations for the language, and I plan to spend a lot of
time changing the syntax and semantics as my ideas about language design evolve. If you find the language interesting,
feel free to give it a try!

## Commands

Compile/run a `.tao` file

```
cargo run -- <FILE>
```

Run compiler tests

```
cargo test
```

Compile/run the standard library

```
cargo run -- lib/std.tao
```

## Compiler arguments

- `--opt`: Specify an optimisation mode (`none`, `fast`, `size`)

- `--debug`: Enable debugging output for a compilation stage (`tokens`, `ast`, `hir`, `mir`, `bytecode`)
