# Bachelor Project - Vrije Universiteit Amsterdam - 2021

## Graph library for Lean 4

This is a graph library for the upcoming 4th version of Lean. Functions are documented in comments in the source code.

*Please note that due to an error in Lean 4 the use of breadth-first search or other algorithms using it may cause an out of bounds error at the start of runtime. This is a [known bug in Lean](https://github.com/leanprover/lean4/issues/534) that will hopefully get fixed soon. Until then you can place this line of code above your main function to fix it: `set_option compiler.extract_closed false in`*

## Example use

To help you get started with using this library see [this](https://github.com/PeterKementzey/example-lean4-pkg-using-Graph) example project I made.

## Benchmarks

Some of the functionality has been benchmarked against Haskell implementations, mainly to compare the performance of the languages. You can find that [here](https://github.com/PeterKementzey/lean-graph-benchmarking).
