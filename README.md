# inclgraph

This is a tool to extract the include graph of a C/C++ codebase (i.e. which source files include which header files, and so on, recursively) and to generate a graphical representation of it using GraphViz.

## To build

`cabal install ./inclgraph.cabal`

## To run

`inclgraph main.c -I/include/path`

## Example

The include graph of msdosfs support in FreeBSD 10.1 looks like this:

`./inclgraph ~/code/usr/src/sys/fs/msdosfs/*.c -I ~/code/usr/src/sys`

![Include graph of msdosfs](https://github.com/lsartran/inclgraph/blob/master/example/freebsd-msdosfs.pdf)
