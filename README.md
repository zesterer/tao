# Bread

Bread is a statically-typed pure function programming language.

## Example

```
# Quicksort
def sort = l ->
    if l:len <= 1
    then l
    else
        let mid = l:nth(l:len / 2) in
        l:filter(x -> x < mid):sort ++
        l:filter(x -> x = mid) ++
        l:filter(x -> x > mid):sort

[45, 75, 98, 24, 10, 12, 60, 32, 17, 41]:sort
```

See `examples/` for more example programs.

## Features

- Functional and pure
- Currying
- Static type system
- Hindley-Milney type inference
- Useful error messages
- Bytecode compilation
- Pattern matching (incomplete)
- Sum types (incomplete)
- Type parameters (incomplete)
- Monadic I/O (incomplete)

## Static typing

Bread has a static type system and type inference.
It supports complex types such as functions and lists.
Below are some examples of types that can be represented in Bread.

- `Num` / `String` / `Bool`

- `Num -> Num` / `String -> Bool -> Num` / `(Num -> Num) -> Bool`

- `List Num` / `List String`

- `Num -> List Num -> Bool`
