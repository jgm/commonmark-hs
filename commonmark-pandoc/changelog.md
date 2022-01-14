# Changelog for commonmark-pandoc

## 0.2.1.2

- Fix addition of sourcepos attributes to blocks (jgm/pandoc#7769).
  We were always adding an enclosing Div, even when the block
  admits attributes. Now the attributes are added to the block
  itself, unless it can't accept attributes (as with inlines).

## 0.2.1.1

- Don't collapse Para to Plain in task lists (#77).

## 0.2.1

- commonmark-pandoc.cabal: remove unneeded build-depend on containers.
- Support wikilinks extension.

## 0.2.0.1

- Allow pandoc-types 1.22.

## 0.2.0.0

- Add HasQuoted instance.
- Apply attributes directly to Table element (#47).

## 0.1.0.0

- Initial release
