# MiniTriangle Compiler

A simple compiler for a fragment of the MiniTriangle language, written in Haskell.
The compiler was written for demonstration purposes while I was a teaching assistant on the Compilers module at the [School of Computer Science, University of Nottingham](https://www.nottingham.ac.uk/computerscience/).

```
> ./mtc examples/fact.mt --ast
Program
├── Declarations
│   ├── Initialise
│   │   ├── n
│   │   └── Literal ── 0
│   ├── Initialise
│   │   ├── x
│   │   └── Literal ── 0
│   └── Initialise
│       ├── i
│       └── Literal ── 0
└── Block
    ├── GetInt ── n
    ├── If
    │   ├── BinaryOp
    │   │   ├── <
    │   │   ├── Variable ── n
    │   │   └── Literal ── 0
    │   ├── Assign
    │   │   ├── x
    │   │   └── Literal ── 0
    │   └── Assign
    │       ├── x
    │       └── Literal ── 1
    ├── Assign
    │   ├── i
    │   └── Literal ── 2
    ├── While
    │   ├── BinaryOp
    │   │   ├── <=
    │   │   ├── Variable ── i
    │   │   └── Variable ── n
    │   └── Block
    │       ├── Assign
    │       │   ├── x
    │       │   └── BinaryOp
    │       │       ├── *
    │       │       ├── Variable ── x
    │       │       └── Variable ── i
    │       └── Assign
    │           ├── i
    │           └── BinaryOp
    │               ├── +
    │               ├── Variable ── i
    │               └── Literal ── 1
    └── PrintInt ── Variable ── x
```

```
> ./mtc example.exp
TAM code written to file examples/fact.tam
> ./mtc examples/fact.tam
GETINT> 10
3628800
HALTED
```

## Description

The compiler parses programs generated by the following context-free grammar:

```
PROGRAM      ::= let DECLARATIONS in COMMAND
DECLARATIONS ::= DECLARATION | DECLARATION ; DECLARATIONS
DECLARATION  ::= var IDENTIFIER | var IDENTIFIER := EXPRESSION
COMMANDS     ::= COMMAND | COMMAND ; COMMANDS
COMMAND      ::= IDENTIFIER := EXPRESSION
               | if EXPRESSION then COMMAND else COMMAND
               | while EXPRESSION do COMMAND
               | getint ( IDENTIFIER )
               | printint ( EXPRESSION )
               | begin COMMANDS end
```

Identifiers are non-empty alphanumeric strings which begin with a letter.
Expressions are integers and integer variables (Boolean operators interpret 0 as false and any other value as true) combined using these connectives (listed from lowest precedence to highest):

| Precedence Level | Syntax                                                   | Associativity                                                |
| ---------------- | -------------------------------------------------------- | ------------------------------------------------------------ |
| Conditional      | `x ? y : z`                                              | none (i.e. nested conditionals must be explicitly bracketed) |
| Disjunctive      | <code>x &#124;&#124; y</code>                            | left                                                         |
| Conjunctive      | `x && y`                                                 | left                                                         |
| Relational       | `x == y`, `x != y`, `x < y`, `x <= y`, `x > y`, `x >= y` | none                                                         |
| Additive         | `x + y`, `x - y`                                         | left                                                         |
| Multiplicative   | `x * y`, `x / y`                                         | left                                                         |
| Negative         | `-x` (integer), `!x` (Boolean)                           | unary                                                        |

The target language is a subset of the Triangle Abstract Machine (TAM) assembly language, which is executed by a stack machine.
The compiler uses the instructions:
* `LOADL n`: push the integer literal `n` onto the stack;
* `LOAD [a]`, `STORE [a]`: load or store a value at index `a` of the stack (the base is index 0).
* `NOT`, `NEG`: apply the corresponding unary operator to the element at the top of the stack, replacing it by the result;
* `AND`, `OR`, `EQL`, `LSS`, `GTR`, `ADD`, `SUB`, `MUL`, `DIV`: apply the corresponding binary operator to the two elements at the top of the stack, replacing them by the result;
* `#label`: a named label representing a position in the code that control-flow instructions may jump to;
* `JUMP #label`, `JUMPIFZ #label`: control-flow instructions which jump to a label unconditionally, or iff the result of popping the stack is zero;
* `GETINT`, `PUTINT`: input/output instruction which read a value from `stdin` and push it to the stack, or pop a value from the stack and print it to `stdout`;
* `HALT`: halt the machine's execution.

Operators in the expression language that are not implemented directly by a TAM instruction can are realised by combining multiple instructions:
* TAM code for `x ? y : z` is generated as if the expression were `!!x * y + !x * z`;
* `x <= y` and`x >= y`are implemented as `!(x > y)` and `!(x < y)`, respectively.

## Usage

The compiler takes a file path and (optionally) a mode from its command-line arguments:

```
./mtc file/path.mt  [MT  mode]
./mtc file/path.tam [TAM mode]
```

The modes available for MT files are:
* `--compile`:   compile an MT program to TAM code and write it to a file (default);
* `--ast`:       print the abstract syntax tree of a program as a tree;
* `--cst`:       print the concrete syntax tree[^1] of a program as a tree;
* all of the TAM modes are also available for MT files, which will be implicitly compiled first.

[^1]: The compiler's parser only constructs the program's AST, so the displayed CST is not strictly the parse tree of the source file (e.g. redundant parentheses will not be displayed).

The modes available for TAM files:
* `--run`:       execute a TAM program to its final stack (default).
