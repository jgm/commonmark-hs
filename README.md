# commonmark

[![travis build status](https://img.shields.io/travis/jgm/commonmark-hs.svg)](https://travis-ci.org/jgm/commonmark-hs)

This repository contains four packages:

- [`commonmark`](commonmark/):
  an umbrella library containing `commonmark-core` and `commonmark-lucid`.

- [`commonmark-core`](commonmark-core/):
  a pure Haskell library for parsing commonmark,
  designed for flexibility and extensibility.

- [`commonmark-lucid`](commonmark-lucid/):
  type instances for parsing commonmark as lucid Html.

- [`commonmark-cli`](commonmark-cli/): a
  command-line program that uses this library to convert
  and syntax-highlight commonmark documents.
