# Welcome to Tao

Tao is a functional, pure, statically-typed programming language.

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

Tao, by default, exists as a REPL that permits you to evaluate expressions.

```
$ tao
>> 4 + 5
9
```

You may also execute Tao code from files by passing their path as the first argument.

```
$ tao hello.tao
"Hello, world!"
```

Tao code written in code form is strictly more powerful (in fact, REPL Tao does not support recursion and so is not Turing-complete).
