# MiniTriangle Compiler

A simple compiler for a fragment of the MiniTriangle (MT) language, targeting the Triangle Abstract Machine (TAM), written in Haskell.
The compiler was written for demonstration purposes while I was a teaching assistant on the Compilers module at the [School of Computer Science, University of Nottingham](https://www.nottingham.ac.uk/computerscience/).

## Usage

The compiler takes a file path and (optionally) a mode from its command-line arguments:

```
./mtc file/path.mt  [MT  mode]
./mtc file/path.tam [TAM mode]
```

The modes available for MT files are:
* `--compile`: compile an MT program to TAM code and write it to a file (default);
* `--ast`: print the abstract syntax tree of a program as a tree;
* all of the TAM modes are also available for MT files, which will be implicitly compiled first, but not written to a file.

The modes available for TAM files:
* `--run`: execute a TAM program (default).
