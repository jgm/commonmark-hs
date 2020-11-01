# Changelog for commonmark

## 0.1.1

* Export `reverseSubforests` from `Commonmark.Blocks` [API change] (#64).

## 0.1.0.2

* Fix tight/loose list detection with multiple blank lines at end (#56).

## 0.1.0.1

* Set source position when we add a token in gobbleSpaces (#54).
  This fixes a bug in gobbling indented spaces in some nested contexts.
* Drop support for ghc 7.10/base 4.8.  We need StrictData.
  Move SCC annotations; ghc 8.0.x doesn't support them on declarations.

## 0.1.0.0

* Initial release
