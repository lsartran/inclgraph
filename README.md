# inclgraph

This is a tool to extract the include graph of a C/C++ codebase (i.e. which source files include which header files, and so on, recursively) and to generate a graphical representation of it using GraphViz.

## To build

`cabal install ./inclgraph.cabal`

## To run

`inclgraph main.c -I/include/path`
