# optimal_stack
Compiler for stack manipulation in Michelson

A lot of operations in Michelson are stack manipulation operations that 
decompose structures on the stack and recompose them. This is a compiler
that calculates the optimal sequence of stack operations to achieve
a given result using A\*.

This could be used as a light compilation pass for a JUMBLE macro
that would work like this:

JUMBLE (a,b):c:(d,(e,f)) => (a,f):(c,d)

Would turn a stack of the form on the left into a stack of the form
on the right. a,b,c,d,e,f are symbolic variables.

Currently, this compiler is quite slow because it attempts to find
the *optimal* solution. Here are ways in which it could be made practical

1. Instead of searching for an optimal solution among the graph
of all stack manipulations, start from a known admissible solution
and refine it using a set of rewriting rules.

2. Speed up the seach by
 - introducing more theorems about minimal costs in order to improve the A\* heuristic
 - adding pseudo-instructions with known costs in order to help guide the search

