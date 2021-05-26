Wikilinks can have one of the following forms:

    [[https://example.org]]
    [[https://example.org|title]]
    [[name of page]]
    [[name of page|title]]

With this version of wikilinks, the title comes after the pipe.

```````````````````````````````` example
[[https://example.org]]
.
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
````````````````````````````````

```````````````````````````````` example
[[https://example.org|title]]
.
<p><a href="https://example.org" title="wikilink">title</a></p>
````````````````````````````````

```````````````````````````````` example
[[Name of page]]
.
<p><a href="Name%20of%20page" title="wikilink">Name of page</a></p>
````````````````````````````````

```````````````````````````````` example
[[Name of page|Title]]
.
<p><a href="Name%20of%20page" title="wikilink">Title</a></p>
````````````````````````````````

