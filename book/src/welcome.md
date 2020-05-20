# Welcome to Tao

Tao is a functional, pure, statically-typed programming language.

## What Tao Is

- **Functional**. Almost everything in Tao is an expression, and functions are first-class citizens.
- **Statically-typed**. The compiler will check during compilation that the types of all expressions are compatible and correct.
  It is also possible to encode invariants about the program into the type system.
- **Smart**. The compiler performs type inference on your code, meaning that you rarely have to manually specify the type of expressions.
  Tao's type inference system is more powerful than the inference that exists in most languages.
- **Elegant**. The syntax is designed to be as concise as possible without hurting readability.
  In addition, there is deliberate symmetry in various aspects of the syntax.
- **Pure**. Side-effects are not permitted in Tao. If a piece of code is given the same inputs twice, it will *always* produce the same output.
  This means that code written in Tao is reliable and predictable.
- **Simple**. Tao has a small number of general constructs that may be combined in a myriad of ways. Power comes from generality, not complexity.
  For someone familiar with functional languages, Tao can be learned in an hour.

## What Tao Is Not

- **A systems programming language**. Tao provides no utilities for manual memory management.
  In addition, many of the core language features require a runtime.
- **A scripting language**. Although the syntax may initially appear somewhat Pythonic, the language has a static type system and is compiled.
- **Finished**. Many aspects of Tao, including the syntax documented in this book, are still in flux.

## Installing

You can find Tao's reference implementation [on GitHub](https://www.github.com/zesterer/tao).
To run it, you must first have Rust installed.
Once installed, you may build Tao with:

```
cargo build
```

Executables will be placed in `target/debug/`. You may also install Tao with:

```
cargo install
```

## Evaluating Code

Tao, by default, exists as a [REPL](https://en.wikipedia.org/wiki/REPL) that alows evaluating expressions.

```
$ tao
>> 4 + 5
9
```

You may also evaluate Tao code from files by passing their path as the first argument.

```
$ tao hello.tao
"Hello, world!"
```

Tao code written in code form is strictly more powerful (in fact, REPL Tao does not support recursion and so is not Turing-complete).

## Comments

In Tao, anything on a line of code following a `#` is considered a comment (unless that `#` is inside a string literal).

```
# This book will contain comments within examples to help explain them.
```
