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
