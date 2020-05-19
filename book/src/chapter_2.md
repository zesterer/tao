# Chapter 2: Complex Expressions

In the last chapter we covered simple expressions such as primitives, lists, and tuples.
In this chapter we will discover how expressions may be manipulated in order to produce useful results.

## Operators

Operators in Tao come in two varieties:

- Unary operators that operate upon a single value
- Binary operators that operate upon two values

Below follows a few examples of common operators.

### Unary operators

Unary operators operate upon a single value.

```
-5
```

The negation operator `-` may be applied to values of type `Num`.

```
!true
```

The not operator '!' may be applied to values of type `Bool`.

### Binary operators

Binary operators operate upon two values.

```
3 + 4
```

The add operator `+` may be applied to values of type `Num`.

```
3 / 4
```

The division operator `/` may be applied to values of type `Num`.

```
true and false
```

The and operator `and` may be applied to values of type `Bool`.

```
5 = 8
```

The equal operator `=` may be applied to a variety of types to compare equivalence. The result is always a `Bool`.

```
[1, 2, 3] ++ [4, 5, 6]
```

The join operator `++` may be applied to list values, provided the types of their elements are consistent.


## Conditionals

It is possible to 'choose' between two different values using an 'if-then-else' expression.
These expressions evaluate to one of two values depending on the truth of a third value, often referred to as the 'predicate'.
Predicates must always be of type `Bool`.

```
if true then 42 else 13
```

This conditional will evaluate to `42`.

```
if false
then [1, 2, 3]
else [4, 5, 6]
```

Conditionals are commonly written on multiple lines to improve clarity.
