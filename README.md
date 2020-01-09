# Bread

Bread is a statically-typed pure function programming language. The interpreter is currently written in Rust.

## Example

```
# Quicksort
let sort = rec(sort -> l ->
    if l:len <= 1
    then l
    else
        let mid = l:nth(l:len / 2) in
        l:filter(x -> x < mid):sort(sort) ++
        l:filter(x -> x = mid) ++
        l:filter(x -> x > mid):sort(sort)
) in

[45, 75, 98, 24, 10, 12, 60, 32, 17, 41]:sort
```

See `examples/` for more example programs.

## Features

- Functional and pure
- Currying
- Bytecode compilation
- Pattern matching (incomplete)
- Sum types (incomplete)
- Type parameters (incomplete)
- Monadic I/O (incomplete)

## Static typing

*TODO*
