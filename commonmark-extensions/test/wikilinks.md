Wikilinks can have one of the following forms:

    [[https://example.org]]
    [[title|https://example.org]]
    [[name of page]]
    [[title|name of page]]

Note that the order of title and page here is that used in
GitHub wikis.  MediaWiki, Obsidian, Foam, and some others use
the opposite order.


```````````````````````````````` example
[[https://example.org]]
.
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
````````````````````````````````

```````````````````````````````` example
[[title|https://example.org]]
.
<p><a href="https://example.org" title="wikilink">title</a></p>
````````````````````````````````

```````````````````````````````` example
[[Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Name of page</a></p>
````````````````````````````````

```````````````````````````````` example
[[Title|Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Title</a></p>
````````````````````````````````

