The `fancy_lists` extension allows various styles of ordered lists:

With period:

```````````````````````````````` example
1. decimal
2. decimal
.
<ol>
<li>decimal</li>
<li>decimal</li>
</ol>
````````````````````````````````

```````````````````````````````` example
A.  upper alpha
B.  upper alpha
.
<ol type="A">
<li>upper alpha</li>
<li>upper alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
a. lower alpha
b. lower alpha
.
<ol type="a">
<li>lower alpha</li>
<li>lower alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
I.  Upper Roman
II.  Upper Roman
.
<ol type="I">
<li>Upper Roman</li>
<li>Upper Roman</li>
</ol>
````````````````````````````````

```````````````````````````````` example
i. Lower Roman
ii. Lower Roman
.
<ol type="i">
<li>Lower Roman</li>
<li>Lower Roman</li>
</ol>
````````````````````````````````

With one parenthesis:

```````````````````````````````` example
1) decimal
2) decimal
.
<ol>
<li>decimal</li>
<li>decimal</li>
</ol>
````````````````````````````````

```````````````````````````````` example
A)  upper alpha
B)  upper alpha
.
<ol type="A">
<li>upper alpha</li>
<li>upper alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
a) lower alpha
b) lower alpha
.
<ol type="a">
<li>lower alpha</li>
<li>lower alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
I)  Upper Roman
II)  Upper Roman
.
<ol type="I">
<li>Upper Roman</li>
<li>Upper Roman</li>
</ol>
````````````````````````````````

```````````````````````````````` example
i) Lower Roman
ii) Lower Roman
.
<ol type="i">
<li>Lower Roman</li>
<li>Lower Roman</li>
</ol>
````````````````````````````````

With two parentheses:

```````````````````````````````` example
(1) decimal
(2) decimal
.
<ol>
<li>decimal</li>
<li>decimal</li>
</ol>
````````````````````````````````

```````````````````````````````` example
(A)  upper alpha
(B)  upper alpha
.
<ol type="A">
<li>upper alpha</li>
<li>upper alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
(a) lower alpha
(b) lower alpha
.
<ol type="a">
<li>lower alpha</li>
<li>lower alpha</li>
</ol>
````````````````````````````````

```````````````````````````````` example
(I)  Upper Roman
(II)  Upper Roman
.
<ol type="I">
<li>Upper Roman</li>
<li>Upper Roman</li>
</ol>
````````````````````````````````

```````````````````````````````` example
(i) Lower Roman
(ii) Lower Roman
.
<ol type="i">
<li>Lower Roman</li>
<li>Lower Roman</li>
</ol>
````````````````````````````````


Note that with Upper Alpha or Upper Roman style list items
followed by periods, we require at least two spaces after the list
marker in order to avoid capturing initials:

```````````````````````````````` example
B.  Russell

B. Russell

I.  J. Good

I. J. Good
.
<ol start="2" type="A">
<li>Russell</li>
</ol>
<p>B. Russell</p>
<ol type="I">
<li>J. Good</li>
</ol>
<p>I. J. Good</p>
````````````````````````````````

A new list starts with any style change:

```````````````````````````````` example
1. one
2) one
.
<ol>
<li>one</li>
</ol>
<ol start="2">
<li>one</li>
</ol>
````````````````````````````````

```````````````````````````````` example
1. one
a. one
.
<ol>
<li>one</li>
</ol>
<ol type="a">
<li>one</li>
</ol>
````````````````````````````````

Variable start numbers should work with all types of lists:

```````````````````````````````` example
b. two

(vi) six
.
<ol start="2" type="a">
<li>two</li>
</ol>
<ol start="6" type="i">
<li>six</li>
</ol>
````````````````````````````````

In cases of ambiguity (such as `'v.'`, which could be
lowercase Roman or lowercase alphabetical, we prefer an
interpretation that continues an existing list:

```````````````````````````````` example
u. one
v. two
.
<ol start="21" type="a">
<li>one</li>
<li>two</li>
</ol>
````````````````````````````````

```````````````````````````````` example
iv. one
v. two
.
<ol start="4" type="i">
<li>one</li>
<li>two</li>
</ol>
````````````````````````````````

When ambiguities cannot be resolved this way,
we prefer to interpret `i.` and `I.` as Roman numerals,
and other single letters as alphabetical:

```````````````````````````````` example
i. one
ii. two
.
<ol type="i">
<li>one</li>
<li>two</li>
</ol>
````````````````````````````````

```````````````````````````````` example
I.  one
.
<ol type="I">
<li>one</li>
</ol>
````````````````````````````````

```````````````````````````````` example
C.  one
CI.  one
.
<ol start="3" type="A">
<li>one</li>
</ol>
<ol start="101" type="I">
<li>one</li>
</ol>
````````````````````````````````
