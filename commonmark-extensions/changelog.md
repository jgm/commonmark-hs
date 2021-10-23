# Changelog for commonmark-extensions

## 0.2.2

  * Add footnote to gfmExtensions.  Note that this also requires
    additional type constraints on gfmExtensions [API change].

## 0.2.1.2

- Fix bug with absolute paths in rebase_relative_paths
  extension on Windows.

## 0.2.1.1

- Fix bug in wikilinks extensions.

## 0.2.1

- Add `rebase_relative_paths` extension.
  New exported module Commonmark.Extensions.RebaseRelativePaths [API change].
- Add `wikilinks_title_before_pipe` and `wikilinks_title_after_pipe`
  extensions (#69).
  New exported module Commonmark.Extensions.Wikilinks [API change].

## 0.2.0.4

- Add a test for autolinks (#66).
- Require commonmark 0.1.1.2 (otherwise autolinks don't work
  properly).

## 0.2.0.3

- Add some new test examples to the autolinks extension spec (#65).
- Allow interior `~` characters in autolinks (#65).

## 0.2.0.2

- Remove unnecessary Typeable constraint on `TaskList` and
  `gfmExtensions` (#58).

- Fix bug in `footnote` extension:  multiple blocks in
  a block container (e.g. block quote or list) inside
  a footnote were being rendered in reverse order (#63,
  Harald Gliebe).

## 0.2.0.1

- Added a missing test file to extra-source-files (#55).

## 0.2.0.0

- Add HasQuoted class in Smart extension, with singleQuoted
  and doubleQuoted methods.  This gives more fleibility in
  supporting smart quotes, and allows us to use pandoc's
  Quoted elements.

- Add advice to haddocks for pipeTableSpec (#52).
  If a line could be a candidate pipe table heading, but the
  following line of separators is not encountered, the line is
  treated as a paragraph, even if it has indications of other
  block-level formatting.  Putting the pipeTableSpec AFTER
  parsers for lists, headings, etc. causes the latter to take
  priority.


## 0.1.0.0

- Initial release
