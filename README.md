# mathematical-expressions-haskell

A simple parser for mathematical expressions in Haskell, using Monadic Parsing.

Based on the following EBNF grammar:

```
expr ::= term (+ expr | - expr | epsilon)

term ::= pow (* term | / term | epsilon)

pow ::= factor (^ pow | epsilon)

factor ::= ( expr ) | integer

integer ::= digit digit*

digit ::= [0-9]
```

This grammar (modified) is due to Graham Hutton's book, "Programming in Haskell" (2nd Edition). Check the companion repository, [mathematical-expressions-java](https://github.com/timmyjose-projects/mathematical-expressions-java) for the comparative implementation in Java, using a Recursive-Descent parser.

## Build and Run

To build the project, execute the following command:

```
$ stack clean && stack build

```

To run the project, execute:

```
$ stack exec mathematical-expressions-haskell-exe

```
