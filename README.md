# commonmark

[![travis build status](https://img.shields.io/travis/jgm/commonmark-hs.svg)](https://travis-ci.org/jgm/commonmark-hs)

This repository contains four packages:

- [`commonmark`](commonmark/):
  a pure Haskell library for parsing commonmark,
  designed for flexibility and extensibility.

- [`commonmark-lucid`](commonmark-lucid/):
  type instances for parsing commonmark as lucid Html.

- [`commonmark-pandoc`](commonmark-pandoc/):
  type instances for parsing commonmark as a Pandoc AST.

- [`commonmark-cli`](commonmark-cli/): a
  command-line program that uses this library to convert
  and syntax-highlight commonmark documents.

See the [`commonmark` README](commonmark/) for a
more detailed description of the project's goals.

