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

This is just like a regular explicit link, except that the
title is treated as a literal string, without markdown formatting:

```````````````````````````````` example
[[title|https://example.org]]
.
<p><a href="https://example.org">title</a></p>
````````````````````````````````

```````````````````````````````` example
[[*title*|https://example.org]]
.
<p><a href="https://example.org">*title*</a></p>
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

A wikilink with an arbitrary title. This form doesn't seem
to be allowed by GitHub, but why not allow it too?

```````````````````````````````` example
[[Title|Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Title</a></p>
````````````````````````````````
