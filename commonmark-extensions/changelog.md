# Changelog for commonmark-extensions

## 0.2.6

  * Track wikilinks with a class instead of the title (Evan
    Silberman). The use of title="wikilink" in HTML output traces
    back to Pandoc's hijacking of the title attribute for this purpose
    back when Pandoc links didn't have Attrs. A coordinated change in
    Pandoc moves this more appropriately into a class.

## 0.2.5.6

  * Autolink parser: track balanced brackets in path (improves
    on #156). We still get a link within a link, which isn't right, but at
    least the link goes to the right place. Cf. jgm/pandoc#10333.

## 0.2.5.5

  * Fix auto_identifiers extension: it now replaces emojis with
    their aliases, as documented.
  * Fix auto_identifiers_ascii extension: it now ignores
    non-ASCII characters with no ASCII equivalent.

## 0.2.5.4

  * Fix autolink parsing regression (#151). This affects autolinks with
    doubled internal line-ending punctuation characters.

## 0.2.5.3

  * Fix rebase_relative_paths extension so it works with URLs with
    non-ASCII characters (#148). Previously these would not be properly
    detected as absolute URIs.

## 0.2.5.2

  * Improve autolinks extension (#147).
    The autolinks extension was interacting badly with explicit links,
    To fix this we had to make autolink parsing a bit stricter than
    the GFM spec does.  They allow unbalanced `)` except at the end
    of a URL (which is defined as: followed by optional final punctuation
    then whitespace or eof).  With this change, we don't allow unbalanced
    `)` at all in raw URLs. This should not be a big problem in practice.

  * Protect against quadratic generated table size explosion (Michael Howell).
    This commit adds a limit to the number of auto-completed cells
    around 200,000. The result is, in these original samples:

## 0.2.5.1

  * Add `test/alerts.md` to extra-source-files in cabal file.

## 0.2.5

  * Add support for alerts extension, supporting GitHub-style alerts (#132).
    <https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#alerts>
    New module Commonmark.Extensions.Alerts [API change].

  * Do not accept footnote labels with line breaks (Michael Howell).

  * Parse `[^ ]` and `[^]` as links (Michael Howell). This is consistent with
    most other CommonMark parsers, even when they have support for footnotes turned on.

## 0.2.4

  * Make `pipe_tables` extension treat backslash escapes like GH does (#112,
    Michael Howell). This change essentially changes the way the text
    `\\|` gets parsed inside a table. In the old version, the first backslash
    escapes the second backslash, and then the pipe is treated as a cell
    separator. In the new version, the pipe is still preceded by a backslash,
    so it is still literal text. The escaping rule is documented in detail
    in the spec for this extension. This change also aligns our escaping
    of pipes with GitHub's.

## 0.2.3.6

  * Fix pipe table parser so that `|`s don't interfere with
    other block structures (Michael Howell, #111, fixing #52 and
    #95). This parser is structured as a system that parses the
    *second* line first, then parses the first line. That is, if
    it detects a delimiter row as the second line of a
    paragraph, it converts the paragraph into a table. This
    seems counterintuitive, but it works better than trying to
    convert a table into a paragraph, since it might need to be
    something else.

  * Improve parsing of inline math (#110).

## 0.2.3.5

  - Resolve entities inside wikilinks (#105, Michał Kukieła).

## 0.2.3.4

  - Require whitespace after definition list marker (#104).
    Otherwise we can inadvertently clobber strikeout or subscript.

## 0.2.3.3

  - Fix definition_lists extension (#96). We were not properly consuming
    indentation in definitions, which caused problems when the definitions
    themselves contained lists.

## 0.2.3.2

- Update lower version bounds for commonmark (#93, David
  Thrane Christiansen).

## 0.2.3.1

- `math` extension:  don't fail when display math contains
  embedded inline math.  See jgm/pandoc#7942.
- Make math parsing more sophisticated.
  Count embeddings inside `{..}`, since math can contain
  e.g. `\text{...}` which itself contains math delimiters.
- Small improvement in pipe table parsing.
  The old parser failed on some edge cases with extra whitespace
  after pipes (which we should just ignore).
- `fancy_list` extension: improve list type ambiguity resolution (#89).

## 0.2.3

- Allow bare word attribute in fenced_divs (#84).  This follows a similar
  change in pandoc (jgm/pandoc#7242).

## 0.2.2.1

- Fix commonmark-extensions to build with GHC 9.2 (#81, Joseph C. Sible).
  Currently `--allow-newer` is needed.

## 0.2.2

- Add footnote to gfmExtensions.  Note that this also requires
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
