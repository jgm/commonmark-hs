# Changelog for commonmark

## 0.2.3

  * Re-export Text.Parsec.Pos from Commonmark.Types (Fraser
    Tweedale, #106).

## 0.2.2

  * Blocks: export `getParentListType` [API change].
  * Require unicode-data >= 0.3.
  * Change `mkFormattingSpecMap` so it integrates different
    FormattingSpecs that use the same character (#87).  Otherwise
    we have problems if you have one formatting spec that
    reacts to single delimiters and another that reacts to
    pairs; if the first fails to match, the fallback behavior
    is produced and the second never matches.
  * Use unicode-data's faster versions of Data.Char functions.
    This speeds up benchmarks for tokenize considerably; little difference
    in other benchmarks.  unicode-data is already a transitive dependency,
    via unicode-transforms.
  * Increase strictness in tokenize/go.
  * Remove legacy cpp needed to support ghc < 8.4.


## 0.2.1.1

  * Fix bug in `prettyShow` for `SourceRange` (#80).
    The bug led to an infinite loop in certain cases.

## 0.2.1

  * Use official 0.30 spec.txt.
  * Update HTML block parser for recent spec changes.
  * Fix test case from commonmark/cmark#383.  We need to index the list
    of stack bottoms not just by the length mod 3 of the closer but by
    whether it can be an opener, since this goes into the calculation of
    whether the delimiters can match.

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
