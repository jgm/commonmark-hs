Wikilinks can have one of the following forms:

    [[https://example.org]]
    [[https://example.org|title]]
    [[name of page]]
    [[name of page|title]]

With this version of wikilinks, the title comes after the pipe.

```````````````````````````````` example
[[https://example.org]]
.
<p><a class="wikilink" href="https://example.org">https://example.org</a></p>
````````````````````````````````

```````````````````````````````` example
[[https://example.org|title]]
.
<p><a class="wikilink" href="https://example.org">title</a></p>
````````````````````````````````

```````````````````````````````` example
[[Name of page]]
.
<p><a class="wikilink" href="Name%20of%20page">Name of page</a></p>
````````````````````````````````

```````````````````````````````` example
[[Name of page|Title]]
.
<p><a class="wikilink" href="Name%20of%20page">Title</a></p>
````````````````````````````````

HTML entities are recognized both in the name of page and in the link title.

```````````````````````````````` example
[[Gesch&uuml;tztes Leerzeichen|&#xDC;ber &amp;nbsp;]]
.
<p><a class="wikilink" href="Gesch%C3%BCtztes%20Leerzeichen">Über &amp;nbsp;</a></p>
````````````````````````````````
