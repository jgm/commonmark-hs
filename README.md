# commonmark

[![CI
tests](https://github.com/jgm/commonmark-hs/workflows/CI%20tests/badge.svg)](https://github.com/jgm/commonmark-hs/actions)

This repository contains four packages:

- [`commonmark`](commonmark/):
  a pure Haskell library for parsing commonmark,
  designed for flexibility and extensibility.
  [![hackage release](https://img.shields.io/hackage/v/commonmark.svg?label=hackage)](http://hackage.haskell.org/package/commonmark)

- [`commonmark-pandoc`](commonmark-pandoc/):
  type instances for parsing commonmark as a Pandoc AST.
  [![hackage release](https://img.shields.io/hackage/v/commonmark-pandoc.svg?label=hackage)](http://hackage.haskell.org/package/commonmark-pandoc)

- [`commonmark-extensions`](commonmark-extensions/):
  a set of useful extensions to core commonmark syntax.
  [![hackage release](https://img.shields.io/hackage/v/commonmark-extensions.svg?label=hackage)](http://hackage.haskell.org/package/commonmark-extensions)

- [`commonmark-cli`](commonmark-cli/): a
  command-line program that uses this library to convert
  and syntax-highlight commonmark documents.
  [![hackage release](https://img.shields.io/hackage/v/commonmark-cli.svg?label=hackage)](http://hackage.haskell.org/package/commonmark-cli)

See the [`commonmark` README](commonmark/) for a
more detailed description of the project's goals.

