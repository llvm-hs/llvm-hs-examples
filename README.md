LLVM Haskell Examples
=====================

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs-examples.svg?branch=master)](https://travis-ci.org/llvm-hs/llvm-hs-examples)

Simple examples demonstrating the usage of the
[llvm-hs](https://github.com/llvm-hs/llvm-hs) for generating and manipulating
LLVM from Haskell.

* [basic](./basic) - Generating LLVM AST and rendering Textual IR
* [orc](./orc) - JIT Compiling IR on the Eager and Lazy ORC Jit using Compile-On-Demand
* [arith](./arith) - a minimal JIT compiler for functions of one (real) variable using recursion schemes
* [irbuilder](./irbuilder) - Basic usage of the LLVM IRBuilder for constructing modules

These examples require LLVM 9.0. Check that your installed LLVM version is
precisely 9.0. If not then follow the install directions in the
[llvm-hs](https://github.com/llvm-hs/llvm-hs) repository.

```bash
$ llvm-config --version
9.0
```

To run the examples with Stack:

```bash
$ stack exec basic
$ stack exec orc
$ stack exec arith
$ stack exec irbuilder
```

To load the examples in GHCI:

```bash
$ stack repl examples:basic
$ stack repl examples:orc
$ stack repl examples:arith
$ stack repl examples:irbuilder
```

To run the examples with Cabal:

```bash
$ cabal run basic
$ cabal run orc
$ cabal run arith
$ cabal run irbuilder
```

License
-------

MIT License
Copyright (c) 2017-2020, Stephen Diehl
