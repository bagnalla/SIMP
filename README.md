# Simp
Solver-aided Imp

A simple imperative-style language with automatic verification.

## Prerequisites
* [Stack](https://docs.haskellstack.org/en/stable/README/) (>= 1.4.0)
* [Z3](https://github.com/Z3Prover/z3)

## Build instructions
* Type 'stack build'

## Run instructions
* Type 'stack exec Simp-exe <input_file>'

## About
Assertions are verified by compiling expressions to logical constraints
and passing them to the Z3 theorem prover.

See src/Ast.hs for the abstract syntax, and one of the ex*.imp example
programs for concrete syntax.

Functions are not recursive and can only refer to other functions that are
defined earlier in the file.

Since IMP commands don't return values, functions require an annotation
to specify the output variable of the function. The value of that
variable at the end of the function's execution is used as its return
value.

The 'eval' block must contain a single arithmetic expression, but it can
call into a 'main' function or something similar. See ex3.imp for an
example.

The verify block can't be empty but 'SKIP' can be used in the absence
of anything else.
