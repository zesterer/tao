# Common Intermediate Representation

Intended as the main intermediate representation upon which most optimisations occur.

The general idea:

- CFG composed of basic blocks
- Every block finishes with a branch (specifically, a match expression)
- Every block declares its dependencies and passes them on to branch targets
- Each statement is three-address code

## Example lowering

```
foo(!a, if -b + c > 0 then bar else baz)
```

becomes

```
b0:
    let v0 = !a in
    let v1 = foo(v0) in
    let v2 = -b in
    let v3 = v2 + c in
    match v3 > 0 in
        | True => :b0
        \ False => :b1
b1:
    let v4 = v1(bar) in
    v4
b2:
    let v4 = v1(baz) in
    v4
```
