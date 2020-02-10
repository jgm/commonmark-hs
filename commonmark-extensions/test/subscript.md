## Subscript

Basic subscript is between `~`s:

```````````````````````````````` example
E=mc~2~.
.
<p>E=mc<sub>2</sub>.</p>
````````````````````````````````

Backslash escapes:

```````````````````````````````` example
E=mc\~2\~.
.
<p>E=mc~2~.</p>
````````````````````````````````

Spaces and formatting are allowed:

```````````````````````````````` example
E=mc~2 or *3*~.
.
<p>E=mc<sub>2 or <em>3</em></sub>.</p>
````````````````````````````````

Punctuation is ignored for purposes of determining
flankingness:

```````````````````````````````` example
E=mc~!~.
.
<p>E=mc<sub>!</sub>.</p>
````````````````````````````````

Subscript can't begin or end with a space:

```````````````````````````````` example
E=mc~ 2~.

E=mc~2 ~.

~ ~
.
<p>E=mc~ 2~.</p>
<p>E=mc~2 ~.</p>
<p>~ ~</p>
````````````````````````````````

