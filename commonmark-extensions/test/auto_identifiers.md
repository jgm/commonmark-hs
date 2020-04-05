## Auto identifiers (extension)

The `auto_identifiers` extension causes identifiers to be
added to headings that lack them.  Identifiers derive their
names from the heading text.  The following recipe is used
(this derives from the practice on GitHub and is slightly
different from legacy pandoc auto identifiers):

- Render the textual content of the heading, without
  any formatting (tags)
- Strip leading and trailing space
- Convert to lowercase
- Convert spaces into hyphens
- Remove all punctuation except `-`, `_`, and punctuation in
  the categories NonSpacingMark, SpacingCombiningMark,
  EnclosingMark, and ConnectorPunctuation.
- Replace emojis with their textual aliases

```````````````````````````````` example
#   Heading  with_two_spaces! 
.
<h1 id="heading--with_two_spaces">Heading  with_two_spaces!</h1>
````````````````````````````````

```````````````````````````````` example
Heading  with_two_spaces!
-------------------------
.
<h2 id="heading--with_two_spaces">Heading  with_two_spaces!</h2>
````````````````````````````````

Auto identifiers are not assigned to headings
that have explicit identifiers:

```````````````````````````````` example
# Heading {#foo}
.
<h1 id="foo">Heading</h1>
````````````````````````````````

```````````````````````````````` example
{#foo}
# Heading
.
<h1 id="foo">Heading</h1>
````````````````````````````````

Auto identifiers are not assigned to non-headings:

```````````````````````````````` example
Hi
.
<p>Hi</p>
````````````````````````````````

Numerical suffixes will be added to avoid
duplicate identifiers:

```````````````````````````````` example
# Hi
# Hi
# Hi
.
<h1 id="hi">Hi</h1>
<h1 id="hi-1">Hi</h1>
<h1 id="hi-2">Hi</h1>
````````````````````````````````

Deduplication should work also when auto identifiers
are mixed with explicit identifiers:

```````````````````````````````` example
# Hi
# Hi {#hi}
# Hi

Hi
==
.
<h1 id="hi-1">Hi</h1>
<h1 id="hi">Hi</h1>
<h1 id="hi-2">Hi</h1>
<h1 id="hi-3">Hi</h1>
````````````````````````````````
