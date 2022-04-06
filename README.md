# Tao

A statically-typed functional language with polymorphism, typeclasses, sum types, pattern-matching, first-class
functions, currying, good diagnostics, and much more!

<a href = "https://www.github.com/zesterer/tao">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/example.png" alt="Demo of Tao's features"/>
</a>

For more example programs, see...

- `examples/hello.tao`: Hello world in Tao
- `examples/adventure.tao`: A text adventure game written in Tao
- `examples/brainfuck.tao`: A brainfuck interpreter written in Tao

## Goals

Right now, Tao is a hobby project and I have no plans to turn it into a production-worthy language. This may change as
the project evolves, but I'd rather spend as much time experimenting with new language features for now. That said, I do
have a few goals for the language itself:

- **Totality**
    - All programs *must* explicitly handle all inputs. There are no mechanisms for panicking, exceptions, etc. The goal
      is to build a type system that's expressive enough to prove the totality of a wide range of programs.
    - In time, I'd like to see the language develop support for *termination analysis* techniques like
      [Walther recursion](https://en.wikipedia.org/wiki/Walther_recursion).

- **Extreme optimisation**
    - A rather dogged and obnoxious opinion of mine is that the 'optimisation ceiling' for statically-typed, total
      functional programming languages is significantly higher than traditional imperative languages with comparably
      weak type systems. I want Tao to be a practical example of this that I can point to rather than deploying nebulous
      talking points about invariants.
    - I've deliberately made sure that the core MIR of Tao has a very small surface area, making it amenable to a
      variety of optimisations and static analyses.

- **Learning**
    - I have only a high-school knowledge of mathematics. I want to use Tao as a test bench to help me learn more about
      mathematics, proofs, type systems, logic, and computation.
    - In addition, I hope that Tao can serve as a useful tool for others looking to get into language design, compiler
      development, or simply functional programming in general.

## Features

- [x] Hindley-Milner type inference
- [x] Useful error messages
- [x] Algebraic data types
    - [x] Sum types
    - [x] Record types
    - [x] Generic data types
    - [x] Nominal aliases (i.e: `data Metres = Real`)
- [x] Type alises
- [x] Type polymorphism via generics
    - [x] Class constraints, including on arbitrary types
    - [x] `where` clauses
- [x] Pattern matching
    - [x] Destructuring and binding
    - [x] ADT patterns
    - [x] List patterns (`[a, b, c]`, `[a, b .. c]`, etc.)
    - [x] Arithmetic patterns (i.e: `n + k`)
    - [x] Inhabitance checks (i.e: `None` exhaustively covers `Maybe Never`)
    - [x] Recursive exhaustivity checks
    - [x] `let` does pattern matching
- [x] First-class functions
    - [x] Functions support pattern-matching
    - [x] Currying
- [x] Typeclasses
    - [x] Associated types
- [x] Built-in lists
    - [x] Dedicated list construction syntax (`[a, b, c]`, `[a, b .. c, d]`, etc.)
- [x] Explicit tail call optimisation
- [x] MIR optimiser
    - [x] Monomorphisation of generic code
    - [x] Inlining
    - [x] Const folding
    - [x] Symbolic execution
    - [x] Dead code removal
    - [x] Exhaustive pattern flattening
    - [x] Unused function pruning
- [x] Bytecode compiler
- [x] Bytecode virtual machine

## Current working on

- [ ] Pattern exhaustivity checking (sound, but unnecessarily conservative)
- [ ] Arithmetic patterns (only nat addition is currently implemented)
- [ ] Typeclasses
    - [ ] Type parameters
    - [ ] More expressive bounds (`where` clauses)
    - [ ] Coherence checker
- [ ] MIR optimiser
    - [ ] Unboxing
    - [ ] Automatic repr changes for recursive types
        - [ ] Transform `data Nat = Succ Nat | Zero` into a runtime integer
        - [ ] Transform `data List A = Cons (A, List A) | Nil` into a vector

## Planned features

- [ ] Better syntax
- [ ] Module system
- [ ] Monadic IO (or an effect system?)
- [ ] Do notation
- [ ] LLVM/Cranelift backend

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
