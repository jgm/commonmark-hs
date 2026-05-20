# TeX Math

Inline math goes between `$` characters, and display math
goes between `$$`:

```````````````````````````````` example
Let $x$ and $y$ be integers such that
$$x=y + 2$$
.
<p>Let <span class="math inline">\(x\)</span> and <span class="math inline">\(y\)</span> be integers such that
<span class="math display">\[x=y + 2\]</span></p>
````````````````````````````````

In inline math, the opening `$` must not be followed by
a whitespace, and the closing `$` must not be
preceeded by whitespace.

```````````````````````````````` example
This is not math: 2000$.
And neither is this $ 4 $.
Or this $4
$.
.
<p>This is not math: 2000$.
And neither is this $ 4 $.
Or this $4
$.</p>
````````````````````````````````

Display math delimiters can be surrounded by whitespace:

```````````````````````````````` example
This is display math:
$$
e=mc^2
$$
.
<p>This is display math:
<span class="math display">\[
e=mc^2
\]</span></p>
````````````````````````````````

Note that math can contain embedded math.  In scanning
for a closing delimiter, we skip material in balanced
curly braces:

```````````````````````````````` example
This is display math:
$$
\text{Hello $x^2$}
$$
And this is inline math:
$\text{Hello $x$ there!}$
.
<p>This is display math:
<span class="math display">\[
\text{Hello $x^2$}
\]</span>
And this is inline math:
<span class="math inline">\(\text{Hello $x$ there!}\)</span></p>
````````````````````````````````


To avoid treating currency signs as math delimiters,
one may occasionally have to backslash-escape them:

```````````````````````````````` example
The cost is between \$10 and 30$.
.
<p>The cost is between $10 and 30$.</p>
````````````````````````````````

Dollar signs must also be backslash-escaped if they
occur within math:

```````````````````````````````` example
$\text{\$}$
.
<p><span class="math inline">\(\text{\$}\)</span></p>
````````````````````````````````

Everthing inside the math construction is treated
as math, and not given its normal commonmark meaning.
```````````````````````````````` example
$b<a>c$
.
<p><span class="math inline">\(b&lt;a&gt;c\)</span></p>
````````````````````````````````

The inline math closer cannot be immediately followed by a digit.
The opener can be, though.
Display math isn't subject to this rule.
```````````````````````````````` example
$1$2$3

$1$2$3$

$1{$2$}3$

$$1$$2$$3

$$1$$2$$3$$

$$1{$$2$$}3$$
.
<p>$1$2$3</p>
<p>$1$2<span class="math inline">\(3\)</span></p>
<p><span class="math inline">\(1{$2$}3\)</span></p>
<p><span class="math display">\[1\]</span>2$$3</p>
<p><span class="math display">\[1\]</span>2<span class="math display">\[3\]</span></p>
<p><span class="math display">\[1{$$2$$}3\]</span></p>
````````````````````````````````
