Attributes have the following syntax:

    attributes <- '{' whitespace* attribute (whitespace attribute)*
                      whitespace* '}'
    attribute <- id_attribute | class_attribute | kv_attribute
    id_attribute <- '#' letter (alphanum | '-' | '_' | ':' | '.')*
    class_attribute <- '.' letter (alphanum | '-' | '_')*
    kv_attribute <- attrname '=' attrvalue
    attrname <- (asciiletter | '_' | ':')
                (asciialphanum | '_' | '.' | '-' | ':')*
    attrvalue <- unquotedvalue | quotedvalue
    unquotedvalue <- [^"-=<>`:whitespace:]+
    quotedvalue <- '"' ([^"] | '\' '"')* '"'

**Attributes that occur at the end of the text of
a Setext or ATX heading (separated by whitespace
from the text) affect the heading element.**

```````````````````````````````` example
# Heading {#ident .class key="value value" key2=value2}
.
<h1 id="ident" class="class" data-key="value value" data-key2="value2">Heading</h1>
````````````````````````````````

```````````````````````````````` example
Heading {#ident .class key="value"}
=====
.
<h1 id="ident" class="class" data-key="value">Heading</h1>
````````````````````````````````

Whitespace is tolerated around the delimiters:

```````````````````````````````` example
# Heading {  #ident  .class key="value" }
.
<h1 id="ident" class="class" data-key="value">Heading</h1>
````````````````````````````````

Multiple class attributes are combined:

```````````````````````````````` example
# Heading {.class1 .class2 class="class3"}
.
<h1 class="class1 class2 class3">Heading</h1>
````````````````````````````````

Only the last id attribute is used:

```````````````````````````````` example
# Heading {#id1 #id2 id="id3"}
.
<h1 id="id3">Heading</h1>
````````````````````````````````

Heading attributes can be followed by whitespace, but
otherwise must come at the end of the line.

```````````````````````````````` example
# Foo {#bar}   
.
<h1 id="bar">Foo</h1>
````````````````````````````````

Headings should still work without attributes:

```````````````````````````````` example
# ATX

Setext
------
.
<h1>ATX</h1>
<h2>Setext</h2>
````````````````````````````````

**Attributes that occur after the opening fence
in a fenced code block affect the code block element.**

```````````````````````````````` example
``` {#ident .class key="value value" key2=value2} 
xyz
```
.
<pre id="ident" class="class" data-key="value value" data-key2="value2"><code>xyz
</code></pre>
````````````````````````````````

```````````````````````````````` example
~~~~{#mycode .ruby .number-lines}
xyz
~~~~
.
<pre id="mycode" class="ruby number-lines"><code>xyz
</code></pre>
````````````````````````````````

If any non-space content comes after the attribute spec, the
whole thing is treated as a raw info string.

```````````````````````````````` example
``` {#foo} bar
xyz
```
.
<pre><code class="language-{#foo}">xyz
</code></pre>
````````````````````````````````

Here the attribute spec is at the end, so the
first word provides the info string and the rest is
treated as an attribute.

```````````````````````````````` example
``` bar {#foo}
xyz
```
.
<pre id="foo"><code class="language-bar">xyz
</code></pre>
````````````````````````````````

**Attributes on inline elements must immediately follow the element to
which they belong.**  If they follow a space, then they belong
to the space.

```````````````````````````````` example
`hi`{#ident .class key=value}
.
<p><code id="ident" class="class" data-key="value">hi</code></p>
````````````````````````````````

```````````````````````````````` example
`hi` {#ident .class key=value}
.
<p><code>hi</code><span id="ident" class="class" data-key="value"> </span></p>
````````````````````````````````

The attributes can wrap:

```````````````````````````````` example
`hi`{#ident .class
key=value}
.
<p><code id="ident" class="class" data-key="value">hi</code></p>
````````````````````````````````

**Attributes that occur immediately before a block
element, on a line by themselves, affect that
element.**

```````````````````````````````` example
{.special}
* * * *
.
<hr class="special" />
````````````````````````````````

```````````````````````````````` example
{#foo .special}
bar
.
<p id="foo" class="special">bar</p>
````````````````````````````````

```````````````````````````````` example
{#foo .special}
# Hi
.
<h1 id="foo" class="special">Hi</h1>
````````````````````````````````

When conflicting specifications of the same attribute are given,
the last one takes precedence, except for classes, which are
cumulative.

```````````````````````````````` example
{#id1}
{#id2}
# Heading {#id3}
.
<h1 id="id3">Heading</h1>
````````````````````````````````

```````````````````````````````` example
{#id1}
{#id3}
# Heading
.
<h1 id="id3">Heading</h1>
````````````````````````````````

```````````````````````````````` example
{.class1 k=1}
{.class2 .class3 k=2}
# Heading {.class4}
.
<h1 class="class1 class2 class3 class4" data-k="2">Heading</h1>
````````````````````````````````


**Attributes that occur at the end of a reference
link definition affect links that refer to that
definition.**

```````````````````````````````` example
[foo]

[foo]: bar "title" {#ident .centered .big}
.
<p><a id="ident" class="centered big" href="bar" title="title">foo</a></p>
````````````````````````````````

**Attributes that occur immediately after an inline
element affect that element.**

```````````````````````````````` example
[foo](bar){#ident .class key="value value" key2=value2}
.
<p><a id="ident" class="class" data-key="value value" data-key2="value2" href="bar">foo</a></p>
````````````````````````````````

```````````````````````````````` example
![foo](bar){#ident .centered .big}
.
<p><img id="ident" class="centered big" src="bar" alt="foo" /></p>
````````````````````````````````

```````````````````````````````` example
[foo]{#ident .centered .big}

[foo]: bar
.
<p><a id="ident" class="centered big" href="bar">foo</a></p>
````````````````````````````````

```````````````````````````````` example
*hi*{.underline}
.
<p><em class="underline">hi</em></p>
````````````````````````````````

**Consecutive attribute specifiers may be used,
either for blocks or for inlines.**

```````````````````````````````` example
*hi*{.underline}{#foo}
.
<p><em class="underline" id="foo">hi</em></p>
````````````````````````````````

```````````````````````````````` example
{.special}
{#foo}
* * * *
.
<hr class="special" id="foo" />
````````````````````````````````

**Entities can be used in attribute values.**

```````````````````````````````` example
# Heading {key="v&#97;lue" }
.
<h1 data-key="value">Heading</h1>
````````````````````````````````
