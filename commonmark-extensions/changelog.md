# Changelog for commonmark-extensions

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
