# Tao

[You can now test Tao in the browser!](https://tao.jsbarretto.com/)

A statically-typed functional language with polymorphism, typeclasses, algebraic effects, sum types, pattern-matching,
first-class functions, currying, good diagnostics, and much more!

<a href = "https://www.github.com/zesterer/tao">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/example.png" alt="Demo of Tao's features"/>
</a>

For more example programs, see...

- [`hello.tao`](https://github.com/zesterer/tao/blob/master/examples/hello.tao): Hello world
- [`input.tao`](https://github.com/zesterer/tao/blob/master/examples/input.tao): Demonstrates a more complex example of IO effects
- [`calc.tao`](https://github.com/zesterer/tao/blob/master/examples/calc.tao): A CLI calculator, demonstrating parser combinators
- [`adventure.tao`](https://github.com/zesterer/tao/blob/master/examples/adventure.tao): A text adventure game
- [`brainfuck.tao`](https://github.com/zesterer/tao/blob/master/examples/brainfuck.tao): A brainfuck interpreter
- [`mutate.tao`](https://github.com/zesterer/tao/blob/master/examples/mutate.tao): Mutation expressed as a side effect
- [`polymorphic_effects.tao`](https://github.com/zesterer/tao/blob/master/examples/polymorphic_effects.tao): Example of a higher-order function that's polymorphic over a side effect
- [`quickcheck.tao`](https://github.com/zesterer/tao/blob/master/examples/quickcheck.tao): A very poor implementation of [`quickcheck`](https://en.wikipedia.org/wiki/QuickCheck) in Tao

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
    - Already the MIR optimiser performs quite a lot of optimisations that radically reduce the number of bytecode
      instructions emitted. See below for a list of these.

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
- [x] Type aliases
- [x] Type polymorphism via generics
    - [x] Class constraints
    - [x] Associated type equality constraints
    - [x] Arbitrary `where` clauses (including associated type equality)
    - [x] Lazy associated item inference (`Foo.Bar.Baz.Biz` lazily infers the class at each step!)
    - [x] Type checker is Turing-complete (is this a feature? Probably not...)
    - [x] Variance is properly tracked through both type and effect parameters
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
    - [x] Type parameters
    - [x] Associated types
    - [x] Operators are implemented as typeclasses
- [x] Algebraic effects
    - [x] Effect objects (independent of functions, unlike some languages)
    - [x] Basin and propagation syntax (equivalent to Haskell's `do` notation, or Rust's `async`/`try` blocks)
    - [x] Generic effects
    - [x] Polymorphic effects (no more `try_x` or `async_x` functions!)
    - [x] Effect sets (i.e: can express values that have multiple side effects)
    - [x] Effect aliases
    - [x] Effect handlers (including stateful handlers, allowing expressing effect-driven IO in terms of monadic IO)
    - [x] Effects can be parameterised by both types and other effects
- [x] Built-in lists
    - [x] Dedicated list construction syntax (`[a, b, c]`, `[a, b .. c, d]`, etc.)
- [x] Explicit tail call optimisation
- [x] Optimisation
    - [x] Monomorphisation of generic code
    - [x] Inlining
    - [x] Const folding
    - [x] Symbolic execution
    - [x] Branch commutation
    - [x] Dead code removal
    - [x] Inhabitance analysis
    - [x] Exhaustive pattern flattening
    - [x] Unused function pruning
    - [x] Unused binding removal
    - [x] Arithmetic rewriting / simplification
    - [x] Identity branch removal
- [x] Bytecode compiler
- [x] Bytecode virtual machine

## Current working on

- [ ] Pattern exhaustivity checking (sound, but unnecessarily conservative)
- [ ] Arithmetic patterns (only nat addition is currently implemented)
- [ ] Typeclasses
    - [ ] Coherence checker
    - [ ] Visible member semantics to relax orphan rules
- [ ] MIR optimiser
    - [ ] Unboxing
    - [ ] Automatic repr changes for recursive types
        - [ ] Transform `data Nat = Succ Nat | Zero` into a runtime integer
        - [ ] Transform `data List A = Cons (A, List A) | Nil` into a vector
- [ ] Algebraic effects
    - [ ] Higher-ranked effects (needed for proper `async` support)
    - [ ] Arbitrary resuming/suspending of effect objects
    - [ ] Full monomorphisation of effect objects

## Planned features

- [ ] Better syntax
- [ ] Module system (instead of `import` copy/paste)
- [ ] LLVM/Cranelift backend

## Interesting features

Here follows a selection of features that are either unique to Tao or are uncommon among other languages.

### Generalised algebraic effects

Tao has support for 'generalised algebraic effects'. 'Effects' means that Tao can express the side-effects of functions
(IO, mutation, exceptions, async, etc.) in type signatures. 'Generalised' means that it's possible for you to create
and use your own effects to express whatever your heart desires. 'Algebraic' means that Tao allows code to be generic
over an effect (or set of effects). For example, consider the `map` function, used to apply a function to each element
of a list in turn:

```py
fn map A, B : (A -> B) -> [A] -> [B]
    | _, [] => []
    \ f, [x .. xs] => [f(x) .. map(f, xs)]
```

`map` can be used like so to, for example, double all elements of a list:

```py
[1, 2, 3, 4]
    -> map(fn x => x * 2)

# Result: [2, 4, 6, 8]
```

Most languages, such as Rust, have a function like this. Unfortunately, it breaks down quickly when we want to do
anything even *slightly* different to a 'pure' mapping between elements within the mapping function. For example,
consider a program for which the mapping function is fallible, or requires some asynchronous operation to complete. It's
necessary to do one of two things:

- Have the function 'silently' exit through stack unwinding, as is the case in C#, C++, etc.
- Create a copy of the function that can handle failure like `try_map` as in Rust

*It's worth noting that Haskell mostly solves this problem with monads: but they're frequently unwieldy. Effect systems
and monads have many similarities, but the former works much harder to make them integrate better with regular control
flow.*

In Tao, this problem can be solved by making `map` generic over an effect parameter, like so:

```py
fn map A, B, e : (A -> e ~ B) -> [A] -> e ~ [B]
    | _, [] => []
    \ f, [x .. xs] => [f(x)! .. map(f, xs)!]
```

A few things have changed here.

Firstly, we've introduced an effect parameter, `e`. Secondly, the type signature has changed: the mapper function,
`A -> B`, now has `e` attached to its return type, resulting in `A -> e ~ B`. This is also present in the final return
type of the function, `e ~ [B]`, expressing that the side effects of the function as a whole correspond to those
performed by the mapping function.

Secondly, a `!` operator has appeared within the implementation after calling the mapping function. This is the 'effect
propagation' operator and signals to the compiler that the side effects of `f` should be lifted to the signature of the
function as a whole.

Note that, otherwise, the implementation remains the same: we have not needed to use any complicated machinery to handle
the side effect (as might be the case in a Rust-style `try_map`), just a single additional operator.

As a result of this change, `map` now accepts mapping functions that perform *any* side effect: throwing errors, IO,
yielding values, mutation, and many more. It also accepts function that perform arbitrary combinations of effects, or
those that have no side effects at all (the empty set is still a valid effect set!):

```py
# Yield each element of the list, resulting in a generator
[1, 2, 3, 4]
    -> map(fn x => yield(x)!)!

# Generate an error if any element of the list is `0`
[1, 2, 3, 4]
    -> map(fn x => if x = 0 then err("no zeroes allowed")! else x)!

# Print each element of the list
[1, 2, 3, 4]
    -> map(fn x => print(x)!)!
```

This is the expressive power of algebraic effect systems: we no longer need to worry about
[function colours](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/), hidden
panics/exceptions, or write many versions of a function to handle all kinds of irregular control flow. Because algebraic
effects generalise so well, it also becomes possible to use them to separate out interfaces from implementations in a
composable way, allowing developers to swap out the implementation of even very core APIs (such as filesystem access) as
required without the complexity and awkwardness of intricate callback systems.

In Tao, effects are [kinds](https://en.wikipedia.org/wiki/Kind_(type_theory)), just like types, lifetimes, and constants
in Rust. They're also represented independently of function signatures too, as 'effect objects' (you can think of effect
objects as being like `Future`/`Promise`s, but generalised to all side effects). Because of this, it's possible to use
them in a vast array of contexts.

### Arithmetic patterns

Tao's type system is intended to be completely sound (i.e: impossible to trigger runtime errors beyond 'implementation'
factors such as OOM, stack overflow, etc.). For this reason, subtraction of natural numbers yields a signed integer, not
a natural number. However, many algorithms still require that numbers be counted down to zero!

To solve this problem, Tao has support for performing arithmetic operations within patterns, binding the result. Because
the compiler intuitively understands these operations, it's possible to statically determine the soundness of such
operations and guarantee that no runtime errors or overflows can ever occur. Check out this 100% sound factorial
program!

```py
fn factorial =
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

In Tao, `arg->f` is shorthand for `f(arg)` (function application). Additionally, this prefix syntax can be chained,
resulting in very natural, first-class pipeline syntax.

```py
my_list
    -> filter(fn x => x % 2 == 0) # Include only even elements
    -> map(fn x => x * x)         # Square elements
    -> sum                        # Sum elements
```

### Useful, user-friendly error diagnostics

This one is better demonstrated with an image.

<a href = "https://www.github.com/zesterer/tao">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/error.png" alt="Example Tao error"/>
</a>

Tao preserves useful information about the input code such as the span of each element, allowing for rich error messages
that guide users towards solutions to their programs. Diagnostic rendering itself is done by my crate
[Ariadne](https://www.github.com/zesterer/ariadne).

### Automatic call graph generation.

Tao's compiler can also automatically generate graphviz call graphs of your programs to help you understand them better.
Here's the expression parser + REPL from `examples/calc.tao`. The call graph will automatically ignore utility functions
(i.e: functions with a `$[util]` attribute on them), meaning that even very complex programs suddenly become
understandable.

<a href = "https://raw.githubusercontent.com/zesterer/tao/master/misc/call_graph.svg">
    <img src="https://raw.githubusercontent.com/zesterer/tao/master/misc/call_graph.svg" alt="Call graph of an expression parser in Tao"/>
</a>

## Usage

### Commands

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

### Compiler arguments

- `--opt`: Specify an optimisation mode (`none`, `fast`, `size`)

- `--debug`: Enable debugging output for a compilation stage (`tokens`, `ast`, `hir`, `mir`, `bytecode`)
