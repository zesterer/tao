# Tao

Tao is a statically-typed functional programming language.

[**Read the book**](book/src/SUMMARY.md)

[**Try it out in your browser**](https://tao.jsbarretto.com/)

## Example

# <img src="https://i.imgur.com/qVXyHq3.png" alt="List manipulation in Tao"/>

See [`examples/`] for a selection of example programs.

## Features

- Functional and pure
- Currying
- Static type system
- Hindley-Milney type inference
- Complex types (lists, tuples, functions, data types)
- Simple, context-free syntax
- Useful error messages
- Generics
- Bytecode compilation
- Pattern matching
- Sum types
- Monadic I/O (incomplete)

## Status

Tao is currently in heavy development and many aspects (particularly the
compiler backend) are very unfinished. In addition, the compiler codebase is
undergoing relatively rapid changes. Syntax is also liable to change as the
language evolves.

### What Works

- Type inference
- Recursive definitions
- Declaration of data types
- Generics
- Bytecode compilation
- Bytecode VM execution
- Pattern-matching
- Common expression constructs (`if`, `match`, `let`, etc.)
- Datatypes (sum types and product types)

### What Doesn't Work

- Trait system
- HKTs
- Standard library / prelude
- IO

## Type System

Tao's type system is ML-like and supports:

- Primitives (`Num`, `Char`, `Bool`, etc.)
- Lists
- Tuples
- Functions
- Sum types
- Records

## Error Messages

Tao aims to have useful error messages. Below are a few examples.

```
Error: Type mismatch between 'Num' and 'Str'
-> line 1, column 2
   1 | (x -> x + 3)("test")
        ^           ^^^^^^
```

```
Error: Cannot fully infer type A in [A] -> Num
-> line 1, column 5
   1 | def len A = |xs of [A]| match xs in
           ---
   5 | def main = []:len
                     ^^^
Hint: Specify all missing types
```

```
Error: Match arms are not exhaustive
-> line 7, column 41
   7 |     def head A of [A] -> Maybe A = |xs| match xs {
                                               ^^^^^^^^^^
   8 |         | [head, _, ...] => Just head
   9 |         | [] => Nil
  10 |     }
       ^^^^^
Hint: Case '[_]' is not handled
```

## Compiler Architecture

Tao's implementation is largely a learning exercise for me. As a result, I'm
avoiding the use of pre-made compiler components as much as possible for now.

Tao's compiler is written in Rust and is composed of several distinct stages
that follow the traditional 'pipeline' compiler architecture closely. These
stages are listed below. Note that many are unfinished.

1) **Lexing**: Turns an input string into a series of disassociated tokens
2) **Parsing**: Turns the context-free grammar into an abstract syntax tree
   (AST)
3) **Type inference**: Converts the AST into a fully-typed higher-level
   intermediate representation (HIR) via a brief representation useful for type
   inference
4) **Soundness checking**: Ensures that various aspects of the program are
   well-formed. This includes pattern exhaustion checks, consistency of
   recursive values, etc.
5) **Instantiation**: Takes the HIR representation and converts it into a MIR
   representation where generic functions are instantiated with concrete types
6) **Optimisation**: Analysis is performed on the MIR and optimisations that are
   most easily made in tree form are made
7) **Code generation**: The MIR is converted into a low-level bytecode that may
   be executed by the bytecode VM. Later stages may transpile to other languages
   or even to native machine code.

