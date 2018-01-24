# commonmark

A pure Haskell library for parsing commonmark.

The library is **fully commonmark-compliant** and passes the
test suite.  It is designed to be **customizable and easily
extensible.**  To customize the output, create an
AST, or support a new output format, one need only define some
new typeclass instances.  It is also easy to add new syntax
elements or modify existing ones.

**Accurate information about source positions** is available
for all block and inline elements.  Thus the library can be
used to create an accurate syntax highlighter or
an editor with synced live preview.

Finally, the library has been designed for **robust performance
even in pathological cases**. The parser behaves well on
pathological cases that tend to cause stack overflows or
exponential slowdowns in other parsers, with parsing speed that
varies linearly with input length.

The following optional extensions are provided:

TODO
