# Matching Patterns

In the previous chapter we discovered how definitions may be used to write code
that evaluates recursively. We also provided an example of that which is
required to write a useful recursive function: a *base condition* that
terminates the recursion and allows the program to evaluate. In this chapter we
will discuss more powerful constructs that Tao provides to express conditional
evaluation and manipulate data structures.

## A Perfect Match

In past chapters we've developed a habit of introducing a simple construct and
later explaining how it is in fact just a special case of a more general (and
hence powerful) construct.

This chapter is no different. In addition to 'if-then-else' expressions, Tao
also provides a **`match`** expression. `match` is an extremely powerful construct
that permits us to check a value against one or more potentially invalid
patterns, then perform a different action in each case.

```
match 5 in
| 1 => "one"
| 2 => "two"
| 3 => "three"
| _ => "something else"
```

The input value, specified at the top of the construct, is tested against each
**arm** in turn until a valid match is found. If more than one arm is a valid
match, the first one to appear in the code will be taken.

To ensure that the program remains correctly-formed, `match` expressions are
required to be **exhaustive**. This means that a value of the correct type will
*always* match with at least one of the arms.

Since `match` is a general construct it can also be used to match more complex
values, including with arbitrary nesting. We can use this to reconstruct the
game 'FizzBuzz' as shown below.

*Note that the remainder operator `%` is used to find the remainder after a
number is divided by another.*

```
let x = 15 in
match (x % 3 = 0, x % 5 = 0) in
| (true, true) => "FizzBuzz"
| (true, _) => "Fizz"
| (_, true) => "Buzz"
| (_, _) => "something else"
```

## Match Bindings

`match` isn't just for checking patterns. It can also be used to destructure
values elements and bind them in a similar manner to `let` function parameters.
It is also possible to perform a value check *at the same time* as binding.

```
let axis = |x, y| match (x, y) in
| (x: 0, y: 0) => ("origin", x, y)
| (x: 0, y) => ("y axis", x, y)
| (x, y: 0) => ("x axis", x, y)
| (x, y) => ("point", x, y)
in
axis(5, 0)
```

## List Patterns

You may have noticed a curious exclusion from the data structures we introduced
previously when concerning patterns. List do indeed work with pattern-matching,
but require a special syntax since they may have an arbitrary length.

```
def reverse = |xs| match xs in
| [] => []
| [head, tail: ...] => reverse(tail) ++ [head]

def main = last_of([4, 7, 3, 9])
```

The first arm is relatively self-explanatory: it matches a list with no elements
produces an empty list. This is out *base case*. The other arm is more complex.
It matches a list with *at least* one element, `head`, and then binds the rest
of the list to `tail`.

```
let length_of = |xs| match xs in
| [] => "zero"
| [_] => "one"
| [_, _] => "two"
| [_, _, ...] => "too many for me to count!"
```
