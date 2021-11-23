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

## Commands

Run compiler

```
cargo run -- <FILE>
```

Run compiler tests

```
cargo test
```

Compile standard library

```
cargo run -- lib/std.tao
```

## Compiler arguments

- `--opt`: Specify an optimisation mode (`none`, `fast`, `size`)

- `--debug`: Enable debugging output for a compilation stage (`tokens`, `ast`, `hir`, `mir`, `bytecode`)
