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

[
	45, 75, 98, 24, 10, 12, 60, 32, 17, 41,
	36, 61, 30, 27, 42, 87, 83, 59, 11, 56,
	22, 99, 50, 76,  6, 57, 19, 52,  7, 51,
	44, 16, 29, 93,  4, 38, 78,  0, 49,  8,
	55, 84, 64, 70,  2, 23, 35, 47, 82, 68,
	63,  3, 48, 13, 95, 86, 91, 88, 73, 53,
	15, 39, 90, 74, 65, 81, 31, 40, 71, 85,
	92,  1, 89, 79, 72, 20, 46,  5, 77, 28,
	96, 18, 21,  9, 25, 97, 66, 43, 67, 69,
	33, 14, 54, 58, 80, 34, 37, 62, 26, 94
]:sort
```

See `examples/` for more example programs.

## Static typing

*TODO*