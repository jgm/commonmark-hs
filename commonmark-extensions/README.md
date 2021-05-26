# commonmark-extensions

[![hackage release](https://img.shields.io/hackage/v/commonmark-extensions.svg?label=hackage)](http://hackage.haskell.org/package/commonmark-extensions)

This package provides some syntax extensions for the
commonmark package:

- [`hard_line_breaks`] (treat new lines as hard breaks)
- [`smart`] (smart quotes, dashes, and ellipses)
- [`strikethrough`] (strikethrough)
- [`superscript`] (superscript)
- [`subscript`] (subscript)
- [`math`] (LaTeX math)
- [`emoji`] (emoji)
- [`autolinks`] (autolink bare URLs and email addresses)
- [`pipe_tables`] (pipe tables)
- [`footnotes`] (footnotes)
- [`definition_lists`] (definition lists)
- [`fancy_lists`] (fancy ordered list markers (parentheses, alpha, roman)
- [`task_lists`] (task lists)
- [`attributes`] (attributes for all inline and block elements)
- [`raw_attribute`] (special raw block and inline elements in any format)
- [`bracketed_spans`] (spans of inline elements with attributes)
- [`fenced_divs`] (groups of block elements with attributes)
- [`auto_identifiers`] (automatic generation of identifiers for headings)
- [`auto_identifiers_ascii`] (automatic generation of ASCII identifiers for headings)
- [`implicit_heading_references`] (headings implicitly define link references)
- [`wikilinks_title_before_pipe`] and
  [`wikilinks_title_after_pipe`] (wikilink syntax)

[`pipe_tables`]: test/pipe_tables.md
[`hard_line_breaks`]: test/hard_line_breaks.md
[`smart`]: test/smart.md
[`strikethrough`]: test/strikethrough.md
[`superscript`]: test/superscript.md
[`subscript`]: test/subscript.md
[`math`]: test/math.md
[`emoji`]: test/emoji.md
[`autolinks`]: test/autolinks.md
[`footnotes`]: test/footnotes.md
[`definition_lists`]: test/definition_lists.md
[`fancy_lists`]: test/fancy_lists.md
[`task_lists`]: test/task_lists.md
[`attributes`]: test/attributes.md
[`raw_attribute`]: test/raw_attribute.md
[`bracketed_spans`]: test/bracketed_spans.md
[`fenced_divs`]: test/fenced_divs.md
[`auto_identifiers`]: test/auto_identifiers.md
[`auto_identifiers_ascii`]: test/auto_identifiers_ascii.md
[`implicit_heading_references`]: test/implicit_heading_references.md
[`wikilinks_title_before_pipe`]: test/wikilinks_title_before_pipe.md
[`wikilinks_title_after_pipe`]: test/wikilinks_title_before_pipe.md

