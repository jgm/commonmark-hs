Wikilinks can have one of the following forms:

    [[https://example.org]]
    [[title|https://example.org]]
    [[name of page]]
    [[title|name of page]]


This form is just like a regular autolink. We don't mark it
up as a wikilink, but treat it as a regular link:

```````````````````````````````` example
[[https://example.org]]
.
<p><a href="https://example.org">https://example.org</a></p>
````````````````````````````````

This is just like a regular explicit link. The title part
can contain commonmark formatting.

```````````````````````````````` example
[[title|https://example.org]]
.
<p><a href="https://example.org">title</a></p>
````````````````````````````````

```````````````````````````````` example
[[*title*|https://example.org]]
.
<p><a href="https://example.org"><em>title</em></a></p>
````````````````````````````````
```````````````````````````````` example
[[`tit|le`|https://example.org]]
.
<p><a href="https://example.org"><code>tit|le</code></a></p>
````````````````````````````````

Commonmark escapes are respected:

```````````````````````````````` example
[[title\|and|https://example.org]]
.
<p><a href="https://example.org">title|and</a></p>
````````````````````````````````

This one is a genuine wikilink.  The title attribute is set
to "wikilink" so that it can be treated specially---e.g., in
some cases, the URL will have to be modified to point to a
wiki directory.

```````````````````````````````` example
[[Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Name of page</a></p>
````````````````````````````````

A wikilink with an arbitrary title.

```````````````````````````````` example
[[Title|Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Title</a></p>
````````````````````````````````
The page name is treated as a literal string, with no formatting:

```````````````````````````````` example
[[Name of \* *page*]]
.
<p><a href="Name%20of%20%5C*%20*page*" title="wikilink">Name of \* *page*</a></p>
````````````````````````````````
