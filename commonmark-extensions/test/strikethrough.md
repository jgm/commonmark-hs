## Strikethrough

Basic strikethrough is between two tildes:

```````````````````````````````` example
~~This is *stricken out*~~
.
<p><del>This is <em>stricken out</em></del></p>
````````````````````````````````

One tilde does nothing:

```````````````````````````````` example
~This is nothing~
.
<p>~This is nothing~</p>
````````````````````````````````

Backslash escapes:

```````````````````````````````` example
~~This is \~\~stricken~~
.
<p><del>This is ~~stricken</del></p>
````````````````````````````````

Intraword strikeout:

```````````````````````````````` example
This~~is~~stricken
.
<p>This<del>is</del>stricken</p>
````````````````````````````````

```````````````````````````````` example
~~This~~is~~stricken~~
.
<p><del>This</del>is<del>stricken</del></p>
````````````````````````````````

Punctuation is ignored for purposes of determining
flankingness:

```````````````````````````````` example
Here I strike out an exclamation point~~!~~.
.
<p>Here I strike out an exclamation point<del>!</del>.</p>
````````````````````````````````

Single tildes leave other formatting delimiters available:

```````````````````````````````` example
(~4 GB) and **~75% of the ~29.6 GB.**
.
<p>(~4 GB) and <strong>~75% of the ~29.6 GB.</strong></p>
````````````````````````````````

```````````````````````````````` example
- **not** X (~4 GB) and **not** Y. **~75% of the ~29.6 GB.**
.
<ul>
<li><strong>not</strong> X (~4 GB) and <strong>not</strong> Y. <strong>~75% of the ~29.6 GB.</strong></li>
</ul>
````````````````````````````````

```````````````````````````````` example
~~a~~~ **~b**
.
<p><del>a</del>~ <strong>~b</strong></p>
````````````````````````````````

Residual singleton and double-only delimiters:

```````````````````````````````` example
~a~ **b**
.
<p>~a~ <strong>b</strong></p>
````````````````````````````````

```````````````````````````````` example
x ~~~a~~ **b~**
.
<p>x ~<del>a</del> <strong>b~</strong></p>
````````````````````````````````

```````````````````````````````` example
~~a~~ **b**
.
<p><del>a</del> <strong>b</strong></p>
````````````````````````````````

