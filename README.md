# MiniTriangle Compiler

A simple compiler for a fragment of the MiniTriangle (MT) language, targeting the Triangle Abstract Machine (TAM), written in Haskell.
The compiler was written for demonstration purposes while I was a teaching assistant on the Compilers module at the [University of Nottingham](https://www.nottingham.ac.uk/computerscience/), so it only uses basic libraries, for example defining its own parser combinator library rather than using an existing solution.

## Usage

The compiler takes a file path and (optionally) a mode from its command-line arguments:

```
./mtc file/path.mt  [MT  mode]
./mtc file/path.tam [TAM mode]
```

The modes available for MT files are:
* `--compile`: compile an MT program to TAM code and write it to a file (default);
* `--ast`: print the abstract syntax tree of a program as a tree.

The modes available for TAM files (also available for MT files, which will implicitly be compiled, but not written to a file, first):
* `--run`: execute a TAM program (default).
