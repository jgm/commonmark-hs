### Regression tests

Issue #113: EOL character weirdness on Windows
(Important: first line ends with CR + CR + LF)

```````````````````````````````` example
line1
line2
.
<p>line1</p>
<p>line2</p>
````````````````````````````````

Issue #114: cmark skipping first character in line
(Important: the blank lines around "Repeatedly" contain a tab.)

```````````````````````````````` example
By taking it apart

- alternative solutions
→
Repeatedly solving
→
- how techniques
.
<p>By taking it apart</p>
<ul>
<li>alternative solutions</li>
</ul>
<p>Repeatedly solving</p>
<ul>
<li>how techniques</li>
</ul>
````````````````````````````````

Issue jgm/CommonMark#430:  h2..h6 not recognized as block tags.

```````````````````````````````` example
<h1>lorem</h1>

<h2>lorem</h2>

<h3>lorem</h3>

<h4>lorem</h4>

<h5>lorem</h5>

<h6>lorem</h6>
.
<h1>lorem</h1>
<h2>lorem</h2>
<h3>lorem</h3>
<h4>lorem</h4>
<h5>lorem</h5>
<h6>lorem</h6>
````````````````````````````````

Issue jgm/commonmark.js#109 - tabs after setext header line


```````````````````````````````` example
hi
--→
.
<h2>hi</h2>
````````````````````````````````

Issue #177 - incorrect emphasis parsing

```````````````````````````````` example
a***b* c*
.
<p>a*<em><em>b</em> c</em></p>
````````````````````````````````

Issue #193 - unescaped left angle brackets in link destination

```````````````````````````````` example
[a]

[a]: <te<st>
.
<p>[a]</p>
<p>[a]: &lt;te<st></p>
````````````````````````````````

Issue #192 - escaped spaces in link destination


```````````````````````````````` example
[a](te\ st)
.
<p>[a](te\ st)</p>
````````````````````````````````

Issue #527 - meta tags in inline contexts

```````````````````````````````` example
City:
<span itemprop="contentLocation" itemscope itemtype="https://schema.org/City">
  <meta itemprop="name" content="Springfield">
</span>
.
<p>City:
<span itemprop="contentLocation" itemscope itemtype="https://schema.org/City">
<meta itemprop="name" content="Springfield">
</span></p>
````````````````````````````````

Issue #530 - link parsing corner cases

```````````````````````````````` example
[a](\ b)

[a](<<b)

[a](<b
)
.
<p>[a](\ b)</p>
<p>[a](&lt;&lt;b)</p>
<p>[a](&lt;b
)</p>
````````````````````````````````

Issue commonmark#526 - unescaped ( in link title

```````````````````````````````` example
[link](url ((title))
.
<p>[link](url ((title))</p>
````````````````````````````````

Issue commonamrk#517 - script, pre, style close tag without
opener.

```````````````````````````````` example
</script>

</pre>

</style>
.
</script>
</pre>
</style>
````````````````````````````````

Issue #289.

```````````````````````````````` example
[a](<b) c>
.
<p>[a](&lt;b) c&gt;</p>
````````````````````````````````

Issue #54.

```````````````````````````````` example
1. Point 1
    ```bash
    date
    pwd
    ```
.
<ol>
<li>Point 1
<pre><code class="language-bash">date
pwd
</code></pre>
</li>
</ol>
````````````````````````````````

Issue #56.

```````````````````````````````` example
- a
  - b
  - c



.
<ul>
<li>a
<ul>
<li>b
</li>
<li>c
</li>
</ul>
</li>
</ul>
````````````````````````````````

Issue #67.
```````````````````````````````` example
[test ](http://www.example.com/)
[ test](http://www.example.com/)
.
<p><a href="http://www.example.com/">test </a>
<a href="http://www.example.com/"> test</a></p>
````````````````````````````````

Issue #114.
```````````````````````````````` example
*.*̀.
.
<p>*.*̀.</p>
````````````````````````````````

Issue #115.
```````````````````````````````` example
~~~\
x
.
<pre><code class="language-\">x
</code></pre>
````````````````````````````````

Issue #119
```````````````````````````````` example
[[]](https://haskell.org)

[[][]](https://haskell.org)

[[[][]](https://haskell.org)

[[][][]](https://haskell.org)
.
<p><a href="https://haskell.org">[]</a></p>
<p><a href="https://haskell.org">[][]</a></p>
<p>[<a href="https://haskell.org">[][]</a></p>
<p><a href="https://haskell.org">[][][]</a></p>
````````````````````````````````

Issue #122
```````````````````````````````` example
* x
* -


Test
.
<ul>
<li>x</li>
<li><ul>
<li></li>
</ul></li>
</ul>
<p>Test</p>
````````````````````````````````

Issue #130
```````````````````````````````` example
[my-link]: https://example.com "\
"
[my-link]
.
<p><a href="https://example.com" title="\
">my-link</a></p>
````````````````````````````````

```````````````````````````````` example
[my-link](https://example.com "\
")
.
<p><a href="https://example.com" title="\
">my-link</a></p>
````````````````````````````````


Issue #133
```````````````````````````````` example
  * * xx

      * yy

    zz
.
<ul>
<li><ul>
<li><p>xx</p>
<ul>
<li>yy</li>
</ul></li>
</ul>
<p>zz</p></li>
</ul>
````````````````````````````````

```````````````````````````````` example
  * * xx
      * yy

    zz
.
<ul>
<li><ul>
<li>xx
<ul>
<li>yy</li>
</ul></li>
</ul>
<p>zz</p></li>
</ul>
````````````````````````````````


Issue #139
```````````````````````````````` example
Test <?xml?> <?xml?>

Test <?xml?> x <?xml?>

Test <![CDATA[ x ]]> <![CDATA[ x ]]>

Test <![CDATA[ x ]]> x <![CDATA[ x ]]>

Test <!DOCTYPE html> <!DOCTYPE html>

Test <!DOCTYPE html> x <!DOCTYPE html>

Test <span> <span>

Test <span> x <span>
.
<p>Test <?xml?> <?xml?></p>
<p>Test <?xml?> x <?xml?></p>
<p>Test <![CDATA[ x ]]> <![CDATA[ x ]]></p>
<p>Test <![CDATA[ x ]]> x <![CDATA[ x ]]></p>
<p>Test <!DOCTYPE html> <!DOCTYPE html></p>
<p>Test <!DOCTYPE html> x <!DOCTYPE html></p>
<p>Test <span> <span></p>
<p>Test <span> x <span></p>
````````````````````````````````


Issue #142
```````````````````````````````` example
<!X
>
<!x
>
.
<!X
>
<!x
>
````````````````````````````````

Issue #149
```````````````````````````````` example
[link](\&#33;)

[link](&#33;)
.
<p><a href="&amp;#33;">link</a></p>
<p><a href="!">link</a></p>
````````````````````````````````

Issue #144
```````````````````````````````` example
+ Is this wrapping list tight, or loose?
  * This nested list is definitely tight.
  -


  -
.
<ul>
<li>Is this wrapping list tight, or loose?
<ul>
<li>This nested list is definitely tight.</li>
</ul>
<ul>
<li></li>
<li></li>
</ul>
</li>
</ul>
````````````````````````````````
```````````````````````````````` example
+ Is this wrapping list tight, or loose?
  * This nested list is definitely tight.
  - First item


  -
.
<ul>
<li>Is this wrapping list tight, or loose?
<ul>
<li>This nested list is definitely tight.</li>
</ul>
<ul>
<li>
<p>First item</p>
</li>
<li></li>
</ul>
</li>
</ul>
````````````````````````````````
```````````````````````````````` example
+ Is this wrapping list tight, or loose?
  * This nested list is definitely tight.
  -


  - Second item
.
<ul>
<li>Is this wrapping list tight, or loose?
<ul>
<li>This nested list is definitely tight.</li>
</ul>
<ul>
<li></li>
<li>
<p>Second item</p>
</li>
</ul>
</li>
</ul>
````````````````````````````````
```````````````````````````````` example
+ Is this wrapping list tight, or loose?
  * This nested list is definitely tight.
  - First item


  - Second item
.
<ul>
<li>Is this wrapping list tight, or loose?
<ul>
<li>This nested list is definitely tight.</li>
</ul>
<ul>
<li>
<p>First item</p>
</li>
<li>
<p>Second item</p>
</li>
</ul>
</li>
</ul>
````````````````````````````````


Issue #136
```````````````````````````````` example
[link](`) `x`
.
<p><a href="%60">link</a> <code>x</code></p>
````````````````````````````````

```````````````````````````````` example
[link](`)[link](`) `x`
.
<p><a href="%60">link</a><a href="%60">link</a> <code>x</code></p>
````````````````````````````````

```````````````````````````````` example
[link](<foo bar=">)">) `x`
.
<p><a href="foo%20bar=%22">link</a>&quot;&gt;) <code>x</code></p>
````````````````````````````````

```````````````````````````````` example
[![image](<foo bar=">)">)![image](<foo bar=">)">)](v) `x`
.
<p><a href="v"><img src="foo%20bar=%22" alt="image" />&quot;&gt;)<img src="foo%20bar=%22" alt="image" />&quot;&gt;)</a> <code>x</code></p>
````````````````````````````````

```````````````````````````````` example
[x](`) <a href="`">
.
<p><a href="%60">x</a> <a href="`"></p>
````````````````````````````````
