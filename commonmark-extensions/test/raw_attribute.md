A "raw attribute" is an `=` plus an alphanumeric string, in
braces, like

    {=html5}

If attached to an inline code span, it causes the span to be
interpreted as raw inline content with the specified format.
If attached to a fenced code block, it causes the block to
be interpreted as raw block content with the specified format.
A raw attribute may not occur together with other attributes.


```````````````````````````````` example
``` {=html}
<b>foo</b>
```
.
<b>foo</b>
````````````````````````````````

```````````````````````````````` example
`<b>foo</b>`{=html}
.
<p><b>foo</b></p>
````````````````````````````````

You can't mix regular and raw attributes:

```````````````````````````````` example
``` {=html #id}
<b>foo</b>
.
<pre><code class="language-{=html">&lt;b&gt;foo&lt;/b&gt;
</code></pre>
````````````````````````````````

```````````````````````````````` example
`<b>foo</b>`{=html .bar}
.
<p><code>&lt;b&gt;foo&lt;/b&gt;</code>{=html .bar}</p>
````````````````````````````````
