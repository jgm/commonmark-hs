Fenced divs are containers for sequences of blocks, to
which an attribute can be attached.

A fenced div begins with an opening fence: a line with three or
more consecutive `:` characters, followed by an attribute
specifier, followed by optional whitespace and the end of the
line.

It ends with a closing fence: a line beginning with at
least as many consecutive `:` characters as in the opening
fence, followed by optional whitespace and the end of the line.

If the end of the input (or enclosing block) is encountered
before a closing fence, the fenced div is implicitly closed.

```````````````````````````````` example
::: {#bar .foo}
Hi

> A block quote.
:::
.
<div id="bar" class="foo">
<p>Hi</p>
<blockquote>
<p>A block quote.</p>
</blockquote>
</div>
````````````````````````````````

```````````````````````````````` example
:::: {#bar .foo} 
Hi

> A block quote.
::::::::::::::::::::::::: 
.
<div id="bar" class="foo">
<p>Hi</p>
<blockquote>
<p>A block quote.</p>
</blockquote>
</div>
````````````````````````````````

Fenced divs may be nested.

```````````````````````````````` example
::: {#bar .foo}
Hi
::: {.baz}
> A block quote.
:::
:::
.
<div id="bar" class="foo">
<p>Hi</p>
<div class="baz">
<blockquote>
<p>A block quote.</p>
</blockquote>
</div>
</div>
````````````````````````````````

A fenced div can interrupt a paragraph, without
an intervening blank line.

```````````````````````````````` example
Paragraph text
::: {#bar .foo}
Hi
:::
.
<p>Paragraph text</p>
<div id="bar" class="foo">
<p>Hi</p>
</div>
````````````````````````````````

A fenced div *must* have attributes.

```````````````````````````````` example
:::
Hi
:::
.
<p>:::
Hi
:::</p>
````````````````````````````````

The closing fence must be at leats as long as the opening
fence.

```````````````````````````````` example
::::: {.foo}
Hi
:::
::::::
.
<div class="foo">
<p>Hi
:::</p>
</div>
````````````````````````````````

If the end of the input (or enclosing block) is encountered
before a closing fence, the fenced div is implicitly closed.

```````````````````````````````` example
> ::: {.foo}
> Hi
.
<blockquote>
<div class="foo">
<p>Hi</p>
</div>
</blockquote>
````````````````````````````````

Instead of a normal attribute specifier in curly braces,
a single bare word may be used; it will be treated as a
"class" attribute:

```````````````````````````````` example
::: c_d
Hi
:::
.
<div class="c_d">
<p>Hi</p>
</div>
````````````````````````````````
