## Example lists

The following description is derived from the syntax description
in pandoc's manual.

The special list marker `@` can be used for sequentially numbered
examples. The first list item with a `@` marker will be numbered '1',
the next '2', and so on, throughout the document. The numbered examples
need not occur in a single list; each new list using `@` will take up
where the last stopped. So, for example:

```````````````````````````````` example
(@)  My first example will be numbered (1).
(@)  My second example will be numbered (2).

Explanation of examples.

(@)  My third example will be numbered (3).
.
<ol class="example" type="1">
<li>My first example will be numbered (1).</li>
<li>My second example will be numbered (2).</li>
</ol>
<p>Explanation of examples.</p>
<ol start="3" class="example" type="1">
<li>My third example will be numbered (3).</li>
</ol>
````````````````````````````````

Numbered examples can be labeled and referred to elsewhere in the
document, either before or after:

```````````````````````````````` example
As may be seen in Example @good, ...

(@good)  This is a good example.

As (@good) illustrates, ...
.
<p>As may be seen in Example 1, …</p>
<ol class="example" type="1">
<li>This is a good example.</li>
</ol>
<p>As (1) illustrates, …</p>
````````````````````````````````

The label can be any string of alphanumeric characters, underscores,
or hyphens.

```````````````````````````````` example
(@foo_bar-11) example

See (@foo_bar-11).
.
<ol class="example" type="1">
<li>example</li>
</ol>
<p>See (1).</p>
````````````````````````````````

Note:  continuation paragraphs in example lists must always
be indented four spaces, regardless of the length of the
list marker.  This is because example
labels tend to be long, and indenting content to the
first non-space character after the label would be awkward.

```````````````````````````````` example
@foo. example

    continuation

@bar. example

   not far enough indented
.
<ol class="example" type="1">
<li><p>example</p>
<p>continuation</p></li>
<li><p>example</p></li>
</ol>
<p>not far enough indented</p>
````````````````````````````````
