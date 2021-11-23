# Tao

Tao is a statically-typed functional programming language.

```
data Maybe A =
    | Just A
    | None

def nth A : Nat -> [A] -> Maybe A = fn
    | 0, [x ..] => x
    | n, [_ .. tail] => nth(n - 1)
    | _, _ => None
```

## Commands

Run compiler

```
cargo run -- <ARGS>
```

Run compiler tests

```
cargo test
```
