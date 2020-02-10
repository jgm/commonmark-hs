## Implicit heading references (extension)

The `implicit_heading_references` causes headings to create
reference links to themselves.  One can use this
with the `auto_identifiers` extension, which should be
put first in the list of extensions, or one can
define identifiers for the headings manually by
putting an attribute block before the heading.

```````````````````````````````` example
# Heading

See the [Heading] above.
.
<h1 id="heading">Heading</h1>
<p>See the <a href="#heading">Heading</a> above.</p>
````````````````````````````````

```````````````````````````````` example
{#foo}
# Heading

See the [Heading] above.
.
<h1 id="foo">Heading</h1>
<p>See the <a href="#foo">Heading</a> above.</p>
````````````````````````````````

Explicitly defined references take precedence:

```````````````````````````````` example
# Heading

See the [Heading] above.

[Heading]: foo
.
<h1 id="heading">Heading</h1>
<p>See the <a href="foo">Heading</a> above.</p>
````````````````````````````````

When there are two headings with the same text,
the first takes precedence:

```````````````````````````````` example
# Heading

# Heading

See the [Heading] above.
.
<h1 id="heading">Heading</h1>
<h1 id="heading-1">Heading</h1>
<p>See the <a href="#heading">Heading</a> above.</p>
````````````````````````````````

Empty headings don't create implicit references:

```````````````````````````````` example
#

##   

See [] and [   ].
.
<h1 id=""></h1>
<h2 id="-1"></h2>
<p>See [] and [   ].</p>
````````````````````````````````
