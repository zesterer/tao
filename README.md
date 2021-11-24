# Tao

Tao is a statically-typed functional programming language.

```py
data Maybe A =
    | Just A
    | None

def len A = fn
    | []: [A] => 0
    | [_ .. tail] => 1 + tail:len
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

## Interesting Features

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
def factorial = fn
    | 0 => 1
    | y ~ x + 1 => y * factorial(x)
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
