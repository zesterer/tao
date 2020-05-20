# Bindings and Patterns

In the last chapter we covered ways to manipulate expressions. In this chapter
we will discuss some constructs that Tao provides to structure more complex
programs.

## Bindings

When structuring code it's often difficult to get very far without some abstract
representation of values. Other programming languages may call these abstract
representations 'variables' but in Tao they are named 'bindings'.

The most common way to create a binding is to use a `let` expression.

```
let x = 5 in x
```

This expression first bindings the value `5` to `x`, and then goes on the
evaluate `x`. Obviously, this will just evaluate to `5`.

Although this example is trivial, `let` expressions permit the construction of
complex programs.

```
let input = 10 in
(input - 32) / 1.8
```

This program may be used to convert an inferior unit of measurement to a
superior one.

## Patterns

In the last section we introduced `let` as a way of binding a value to an
identifier. In fact, `let` is considerably more powerful than demonstrated in
previous examples. The general rule for `let` is as follows.

```
let <pattern> = <expression> in <expression>
```

Here, `<pattern>` denotes a construct that may be used to disassemble a complex
value into simpler parts. This is known as 'destructuring'. For example, we can
use `let` to destructure a tuple into its internal elements, binding them to
identifiers at the same time.

```
let (x, y) = (4, 5) in
x + y
```

It is not necessary to bind all elements of a pattern. To ignore an element, the
`_` wildcard may be used. In addition, patterns support nesting.

```
let ((_, b), _) = ((1, 2), (true, false)) in
b
```
