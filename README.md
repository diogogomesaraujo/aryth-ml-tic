# arith-ml-tic

A lightweight OCaml lexer and interpreter for arithmetic functions to consolidate what was learnt in Theory of Computation class.

## Quick Start 
### Install Dependencies
```bash
opam install dune menhir ppx_deriving
```
### Build
```bash
dune build
```
### Run
```bash
dune exec ./bin/main.exe
```

## Supported Operations
- Addition
- Subtraction
- Multiplication
- Division
- Power

## How It Works
- Tokenize input
- Parse tokens
- Interpret results
