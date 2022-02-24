# Tao

A statically-typed functional language with polymorphism, typeclasses, sum types, pattern-matching, first-class
functions, currying, good diagnostics, and much more!

<a href = "https://www.github.com/zesterer/tao">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/example.png" alt="Demo of Tao's features"/>
</a>

## Features

- [x] Hindley-Milner type inference
- [x] Useful error messages
- [x] Sum types
- [x] Records
- [x] Nominal data types
- [x] Union types (anonymous sum types)
- [x] Pattern matching (inc. union patterns)
- [x] First-class functions
- [x] Currying
- [x] Type polymorphism (for both types and values)
- [x] Typeclasses
- [x] Associated types
- [x] Built-in lists
- [x] Monomorphisation
- [x] MIR optimiser
    - [x] Inlining
    - [x] Const folding
    - [x] Symbolic execution
    - [x] Unreachable elision
    - [x] Dead code removal
- [x] Bytecode compiler

## Current working on

- [ ] Pattern exhaustivity checking (sound, but unnecessarily conservative)
- [ ] Arithmetic patterns (only nat addition is currently implemented)
- [ ] Typeclasses (simple implementation done, no compile-time coherence checker, compiler panics on detection of
      incoherence during monomorphisation)

## Planned features

- [ ] Better syntax
- [ ] Module system
- [ ] Monadic IO (or an effect system?)
- [ ] Do notation
- [ ] LLVM/Cranelift backend

## Why?

Tao is primarily a personal hobby project. I have no real aspirations for the language, and I plan to spend a lot of
time changing the syntax and semantics as my ideas about language design evolve. If you find the language interesting,
feel free to give it a try!

## Interesting features

Here follows a selection of features that are either unique to Tao or are uncommon among other languages.

### Arithmetic patterns

Tao's type system is intended to be completely sound (i.e: impossible to trigger runtime errors beyond 'implementation'
factors such as OOM, stack overflow, etc.). For this reason, subtraction of natural numbers yields a signed integer, not
a natural number. However, many algorithms still require that numbers be counted down to zero!

To solve this problem, Tao has support for performing arithmetic operations within patterns, binding the result. Because
the compiler intuitively understands these operations, it's possible to statically determine the soundness of such
operations and guarantee that no runtime errors or overflows can ever occur. Check out this 100% sound factorial
program!

```py
def factorial =
    | 0 => 1
    \ y ~ x + 1 => y * factorial(x)
```

### All functions are lambdas and permit pattern matching

Excluding syntax sugar (like type aliases), Tao has only two high-level constructs: values and types. Every 'function'
is actually just a value that corresponds to an line lambda, and the inline lambda syntax naturally generalises to
allow pattern matching. Multiple pattern arguments are permitted, each corresponding to a parameter of the function.

```py
def five =
    let identity = fn x => x in
    identity(5)
```

### Exhaustive pattern matching

Tao requires that pattern matching is exhaustive and will produce errors if patterns are not handled.

### Very few delimiters, but whitespace *isn't* semantic

In Tao, every value is an expression. Even `let`, usually a statement in most languages, is an expression. Tao requires
no semicolons and no code blocks because of this fact.

### Currying and prefix calling

In Tao, `arg:f` is shorthand for `f(arg)` (function application). Additionally, this prefix syntax can be chained,
resulting in very natural, first-class pipeline syntax.

```py
my_list
    :filter(fn x => x % 2 == 0) # Include only even elements
    :map(fn x => x * x)         # Square elements
    :sum                        # Sum elements
```

### Useful, user-friendly error diagnostics

This one is better demonstrated with an image.

<a href = "https://www.github.com/zesterer/tao">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/error.png" alt="Example Tao error"/>
</a>

Tao preserves useful information about the input code such as the span of each element, allowing for rich error messages
that guide users towards solutions to their programs. Diagnostic rendering itself is done by my crate
[Ariadne](https://www.github.com/zesterer/ariadne).

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
