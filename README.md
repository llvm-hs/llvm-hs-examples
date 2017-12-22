LLVM Haskell Examples
=====================

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs-examples.svg?branch=master)](https://travis-ci.org/llvm-hs/llvm-hs-examples)

Simple examples demonstrating the usage of the
[llvm-hs](https://github.com/llvm-hs/llvm-hs) for generating and manipulating
LLVM from Haskell.

* [basic](./basic) - Generating LLVM AST and rendering Textual IR
* [orc](./orc) - JIT Compiling IR on the Eager and Lazy ORC Jit using Compile-On-Demand
* [arith](./arith) - a minimal JIT compiler for functions of one (real) variable using recursion schemes

To run the examples:

```bash
$ stack exec basic
$ stack exec orc
$ stack exec arith
```

To load the examples in GHCI:

```bash
$ stack repl examples:basic
$ stack repl examples:orc
$ stack repl examples:arith
```

License
-------

MIT
