## Superscript

Basic superscript is between `^`s:

```````````````````````````````` example
E=mc^2^.
.
<p>E=mc<sup>2</sup>.</p>
````````````````````````````````

Backslash escapes:

```````````````````````````````` example
E=mc\^2\^.
.
<p>E=mc^2^.</p>
````````````````````````````````

Spaces and formatting are allowed:

```````````````````````````````` example
E=mc^2 or *3*^.
.
<p>E=mc<sup>2 or <em>3</em></sup>.</p>
````````````````````````````````

Punctuation is ignored for purposes of determining
flankingness:

```````````````````````````````` example
E=mc^!^.
.
<p>E=mc<sup>!</sup>.</p>
````````````````````````````````

Superscript can't begin or end with a space:

```````````````````````````````` example
E=mc^ 2^.

E=mc^2 ^.

^ ^
.
<p>E=mc^ 2^.</p>
<p>E=mc^2 ^.</p>
<p>^ ^</p>
````````````````````````````````
