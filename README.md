# commonmark

[![CircleCI](https://circleci.com/gh/jgm/commonmark-hs.svg?style=svg)](https://circleci.com/gh/jgm/commonmark-hs)
[![CI
tests](https://github.com/jgm/commonmark-hs/workflows/CI%20tests/badge.svg)](https://github.com/jgm/commonmark-hs/actions)

This repository contains three packages:

- [`commonmark`](commonmark/):
  a pure Haskell library for parsing commonmark,
  designed for flexibility and extensibility.

- [`commonmark-pandoc`](commonmark-pandoc/):
  type instances for parsing commonmark as a Pandoc AST.

- [`commonmark-cli`](commonmark-cli/): a
  command-line program that uses this library to convert
  and syntax-highlight commonmark documents.

See the [`commonmark` README](commonmark/) for a
more detailed description of the project's goals.

