# Chapter 5: Definitions and Recursion

In the last chapter, we explained how functions might be built up to abstract
over classes of behaviours. In this chapter, we will finally deviate from the
world of expressions. From here on, you'll need to put any examples you want to
run [into a text file](./welcome.md#evaluating-code) since Tao's REPL only
supports evaluating expressions.

## Definitions

`let` bindings are all well and good, but sometimes we want a more rugged way to
identify expressions in a manner that makes them available to the whole program,
not just code that happens to follow them. Definitions allow this.

```
def one = 1
```

This definition aliases the number `1` and so has type `Num`.

Definitions can reference other definitions, as shown below.

```
def one = 1
def two = one + 1
def three = two + 1
```

In this example, we define an empty list of numbers.

```
def empty_list_of_numbers = []
```

However, upon running the code we encounter the following error.

```
Error: Cannot infer type ? in [?]
-> line 1, column 29
   1 | def empty_list_of_numbers = []
                                   ^^
Hint: Specify all missing types
```

In this case, we've not provided enough information for Tao to know what type
we're expecting the list to have since we've not given any examples of elements
it may contain. To make Tao understand that the list should be known to be a
numerical list, we can insert a **type hint** that gives Tao enough information
to make sense of it.

```
def empty_list_of_numbers of [Num] = []
```

This works great and no more errors are encountered.

## Main

`main` is a definition with a special meaning in Tao. Much like in other
languages, it represents the primary expression that will be evaluated when a
Tao program is run.

```
def main = "Hello, world!"
```

## Recursion

One of the essential components required for a language to be
['Turing-complete'](https://en.wikipedia.org/wiki/Turing_completeness) is the
ability to perform a set of actions again and again, with the ability to change
behaviour on each iteration depending on the value of some state.

Tao doesn't directly support iteration (in Tao, all bindings are immutable - how
might one construct a useful loop without some kind of mutating counter
variable?) but instead supports another construct that is formally equivalent
and can often be much more expressive - **recursion**.

A recursive definition is one that (directly or indirectly) references itself.

```
def meaning_of_life = meaning_of_life
```

This definition is recursive. Attempting to evaluate it will result in Tao
looping forever since there exists no *base condition* upon which the evaluation
may complete. Deep Thought would be proud.

*Incidentally, this definition will also result in an error because
no information about its type has been provided.*

```
def one = two - 1
def two = one + 1
```

These definitions are *mutually recursive*. Although they don't directly
reference themselves, they reference one-another and so *indirectly* reference
themselves.

Recursion becomes useful when combined with a conditional and some input that
can be passed through successive references to the definition. We've already
learned how to do both of these with *conditionals* and *functions*!

```
def factorial = |x| if x = 0
	then 1
	else x * factorial(x - 1)
```

This definition is a recursive function. It recurses until it reaches the base
input, `0`, and then propagates the result of its computation up through each
layer of recursion until it produces a result. In this case, it calculates the
[factorial](https://en.wikipedia.org/wiki/Factorial) of any positive integer.
