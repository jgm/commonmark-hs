# Changelog for commonmark

## 0.2

* Commonmark.Inlines: export LinkInfo(..) [API change].
* Commonmark.Inlines: export pLink [API chage].
* Comonmark.ReferenceMap: Add linkPos field to LinkInfo [API change].
* Commonmark.Tokens: normalize unicode to NFC before tokenizing (#57).
  Normalization might affect detection of flankingness, recognition
  of reference links, etc.
* Commonmark.Html:  add data-prefix to non-HTML5 attributes, as pandoc does.
* Remove unnecessary build-depends.
* Use lightweight tasty-bench instead of criterion for benchmarks.

## 0.1.1.4

* Fix build with GHC 9.0.1 (Simon Jakobi, #72).

## 0.1.1.3

* Fix bug in links with spaces at the beginning or end of
  link description (#67).  We were putting flankingness constraints
  on the link delimiters, but this isn't requried by the spec.

## 0.1.1.2

* Fix bug in fix to #65 (#66).

## 0.1.1.1

* Fixed corner case with link suffix parsing, which could result
  in dropped tokens in certain cases (#65).

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
