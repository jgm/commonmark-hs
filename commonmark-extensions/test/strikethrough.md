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



Single tildes that can't produce strikethrough should not
swallow intervening delimiters (#181):

```````````````````````````````` example
~4 **~75 GB.**
.
<p>~4 <strong>~75 GB.</strong></p>
````````````````````````````````

```````````````````````````````` example
~~a~ ~b~~
.
<p><del>a~ ~b</del></p>
````````````````````````````````
