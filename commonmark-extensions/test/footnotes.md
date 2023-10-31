## Footnotes

TODO: proper spec.
This test is based on the one from gfm.

```````````````````````````````` example
This is some text![^1]. Other text.[^footnote].

Here's a thing[^other-note].

And another thing[^codeblock-note].

This doesn't have a referent[^nope].


[^1]: Some *bolded* footnote definition.

[^footnote]:
    > Blockquotes can be in a footnote.

        as well as code blocks

    or, naturally, simple paragraphs.

[^other-note]:       no code block here (spaces are stripped away)

[^codeblock-note]:
        this is now a code block (8 spaces indentation)

Hi!

[^unused]: This is unused.
.
<p>This is some text!<sup class="footnote-ref"><a href="#fn-1" id="fnref-1">1</a></sup>. Other text.<sup class="footnote-ref"><a href="#fn-footnote" id="fnref-footnote">2</a></sup>.</p>
<p>Here's a thing<sup class="footnote-ref"><a href="#fn-other-note" id="fnref-other-note">3</a></sup>.</p>
<p>And another thing<sup class="footnote-ref"><a href="#fn-codeblock-note" id="fnref-codeblock-note">4</a></sup>.</p>
<p>This doesn't have a referent[^nope].</p>
<p>Hi!</p>
<section class="footnotes">
<div class="footnote" id="fn-1">
<div class="footnote-number">
<a href="#fnref-1">1</a>
</div>
<div class="footnote-contents">
<p>Some <em>bolded</em> footnote definition.</p>
</div>
</div>
<div class="footnote" id="fn-footnote">
<div class="footnote-number">
<a href="#fnref-footnote">2</a>
</div>
<div class="footnote-contents">
<blockquote>
<p>Blockquotes can be in a footnote.</p>
</blockquote>
<pre><code>as well as code blocks
</code></pre>
<p>or, naturally, simple paragraphs.</p>
</div>
</div>
<div class="footnote" id="fn-other-note">
<div class="footnote-number">
<a href="#fnref-other-note">3</a>
</div>
<div class="footnote-contents">
<pre><code>   no code block here (spaces are stripped away)
</code></pre>
</div>
</div>
<div class="footnote" id="fn-codeblock-note">
<div class="footnote-number">
<a href="#fnref-codeblock-note">4</a>
</div>
<div class="footnote-contents">
<pre><code>this is now a code block (8 spaces indentation)
</code></pre>
</div>
</div>
<div class="footnote" id="fn-unused">
<div class="footnote-number">
<a href="#fnref-unused">5</a>
</div>
<div class="footnote-contents">
<p>This is unused.</p>
</div>
</div>
</section>
````````````````````````````````

Ensure that nested blocks in footnotes are rendered in the
right order (#63).

```````````````````````````````` example
Hello[^test]

Footnote containing a list[^list]

[^test]:
    > first
    >
    > second
    >
    > third

[^list]:
    1. First element
    1. Second element
.
<p>Hello<sup class="footnote-ref"><a href="#fn-test" id="fnref-test">1</a></sup></p>
<p>Footnote containing a list<sup class="footnote-ref"><a href="#fn-list" id="fnref-list">2</a></sup></p>
<section class="footnotes">
<div class="footnote" id="fn-test">
<div class="footnote-number">
<a href="#fnref-test">1</a>
</div>
<div class="footnote-contents">
<blockquote>
<p>first</p>
<p>second</p>
<p>third</p>
</blockquote>
</div>
</div>
<div class="footnote" id="fn-list">
<div class="footnote-number">
<a href="#fnref-list">2</a>
</div>
<div class="footnote-contents">
<ol>
<li>First element</li>
<li>Second element</li>
</ol>
</div>
</div>
</section>
````````````````````````````````

Footnote labels cannot be empty, or composed only of whitespace.

```````````````````````````````` example
Test [^] link [^ ]

[^]: https://haskell.org
.
<p>Test <a href="https://haskell.org">^</a> link <a href="https://haskell.org">^ </a></p>
````````````````````````````````

```````````````````````````````` example
[^]: not a footnote

[^ ]: not a footnote

[^
]: not a footnote
.
<p>[^]: not a footnote</p>
<p>[^ ]: not a footnote</p>
<p>[^
]: not a footnote</p>
````````````````````````````````

Footnote labels cannot contain line breaks, even where link labels can.

```````````````````````````````` example
[^foo\
bar]: not a footnote definition

[baz\
quux]: https://haskell.org

[first
second]: https://haskell.org

[^third
fourth]: not a footnote definition

[baz\
quux]
[^foo\
bar]
[first
second]
[^third
fourth]
.
<p>[^foo<br />
bar]: not a footnote definition</p>
<p>[^third
fourth]: not a footnote definition</p>
<p><a href="https://haskell.org">baz<br />
quux</a>
[^foo<br />
bar]
<a href="https://haskell.org">first
second</a>
[^third
fourth]</p>
````````````````````````````````

Only the first line of a footnote's following paragraph needs indented.

```````````````````````````````` example
[^foo]:bar
baz

    quux
arst
    qwfp

[^foo]
.
<p><sup class="footnote-ref"><a href="#fn-foo" id="fnref-foo">1</a></sup></p>
<section class="footnotes">
<div class="footnote" id="fn-foo">
<div class="footnote-number">
<a href="#fnref-foo">1</a>
</div>
<div class="footnote-contents">
<p>bar
baz</p>
<p>quux
arst
qwfp</p>
</div>
</div>
</section>
````````````````````````````````

Lazy continuations require the first line to have text in it,
and to lazily continue a paragraph after the first, it will need to
start with an indented line also.

```````````````````````````````` example
[^foo]:
baz

    quux

[^foo]
.
<p>baz</p>
<pre><code>quux
</code></pre>
<p><sup class="footnote-ref"><a href="#fn-foo" id="fnref-foo">1</a></sup></p>
<section class="footnotes">
<div class="footnote" id="fn-foo">
<div class="footnote-number">
<a href="#fnref-foo">1</a>
</div>
<div class="footnote-contents">
</div>
</div>
</section>
````````````````````````````````
