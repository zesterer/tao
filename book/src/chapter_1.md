# Chapter 1: Values

Perhaps the most fundamental construct in Tao is the **expression**.

Expressions come in many forms but all represent some value that may be evaluated.

All expressions have a **type** that described the properties of the value that the expression will evaluate to.

## Primitives

The simplest expressions are **primitive** values that are implicitly understood by Tao.
A selection of example primitive expressions follows.

```
5
```

This primitive is a number and has type `Num`.
Numbers can be negative or fractional too.

```
true
```

This primitive is a boolean and has type `Bool`.
Booleans have only two possible values, `true` or `false`, and are the simplest of the primitives.

```
"Hello, world!"
```

This primitive is a string and has type `Str`.
Strings can be of any length and may contain any UTF-8 characters.

## Lists

Lists contain many elements of the same type.
Lists have the type `[A]`, where `A` is the type of the list's elements.
Lists may have any length and may be split into fragments or combined together.
We will find out more about this in the next chapter.

```
[1, 2, 3, 4]
```

This is a list of numbers and has type `[Num]`.

```
[true, false, true]
```

This is a list of booleans and has type `[Bool]`.

## Tuples

Tuples contain a fixed number of elements, each with their own type.
The type of each element is encoded in the type of a tuple.

```
(42, true, "foobar")
```

This tuple has 3 elements; `42`, `true`, and `"foobar"`.
Its type is therefore `(Num, Bool, Str)`.

```
([3, 6, 9], [false, true, true])
```

Complex types like tuples and lists may be nested.
This tuple has two elements, each a list containing different types, and therefore has type `([Num], [Bool])`.

```
(4,)
```

This is a tuple with a single element (and is semantically distinct from the element itself).
This syntax is necessary to avoid a syntax conflict with ordinary parentheses.
This tuple has type `(Num,)`

```
()
```

This tuple has no elements and is known as 'unit'. As one might expect, it has type `()`.
