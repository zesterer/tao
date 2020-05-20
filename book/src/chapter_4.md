# Functions

In the last chapter we explained how bindings may be used to abstractly
represent values in a manner that allows the construction of complex programs.
In this chapter we will discover how code may be abstracted in a similar way.

## Inputs, Outputs

Machines take in material inputs, operate upon them, and produce material
outputs. Their digital counterpart, computers, take data inputs, operate upon
them, and produce data outputs.

In the world of mathematics, the equivalent construct is the **function**.
Functions represent some process performed upon input values that results in
output values.

Tao allows functions to be represented using a concise, readable syntax.

```
|x| x
```

This is the simplest possible function. It takes some input, `x`, and then
produces it as the output without changing it in any way. This function is often
known as the 'identity' function.

In Tao, functions are **first-class** citizens. This means that they are values
and hence have a type. The type of functions is `I -> O`, where `I` is the input
type and `O` is the output type. Function inputs are known as **parameters**.

```
|x| x + 1
```

This function adds `1` to its input and has type `Num -> Num`.

Functions my have a parameter 'applied' to them, yielding the result of their
internal operation. The general term for this is **application**.

```
let identity = |x| x in
identity(5)
```

The result of this expression is `5`.

There also exists an infix shorthand for function application using `:`.

```
let double = |x| x * 2 in
42:double
```

The result of this expression is `84`.

```
|x, y| x * y
```

This function takes two inputs and multiplies them together. It has type
`Num -> Num -> Num`.

## Currying

Perhaps the last example struck you as a little confusing.
How can a function have multiple inputs if the type syntax for functions only
permits a single input? And what on earth is `Num -> Num -> Num` supposed to
mean?

Tao supports a feature called **currying**, something often supported by
functional languages.

In languages with currying, all functions take *exactly* one input. To create
functions that behave as if they take multiple inputs, it is necessary to return
a new function from the original.

For example, the last example can also be written as follows.


```
|x| |y| x * y
```

To use this function, it is necessary to apply the first parameter to it, and
then to apply the second parameter to the newly created function. This explains
the bizarre type of the function. `Num -> Num -> Num` should be read as
`Num -> (Num -> Num)`, or 'a function that takes a `Num`, producing a new
function that takes a `Num` and then produces a `Num`'.

Currying may initially appear to be an unnecessary complication, but in fact has
many uses. By allowing a function to be evaluated one parameter at a time, it is
possible to specialise functions and then reuse them later. This technique is
known as **partial application**.

```
# Declare a generic add function
let add = |x, y| x + y in

# 'Specialise' this function into one that adds `5` to its input
let add_five = add(5) in

# Use this new function
add_five(3) + add_five(4)
```

## More Patterns

Tao's functions are in fact even more general than previously mentioned.
Function parameters are in fact permitted to be a pattern, just like a `let`
expression.

The general rule is as follows.

```
|<pattern>| <expression>
```

As previously mentioned, it is also permitted to have multiple parameters
between the `|` pipes, separated by commas. This will
[desugar](https://en.wikipedia.org/wiki/Syntactic_sugar) to multiple nested
functions that may be curried.

This turns out to be extremely powerful. Equipped with our existing knowledge of
tuples, patterns, and functions, we can actually emulate traditional
multi-argument functions if we so wish.

```
let product = |(x, y, z)| x * y * z in
product((3, 4, 5))
```

Of course, this isn't commonly useful. Partial application works just fine!
