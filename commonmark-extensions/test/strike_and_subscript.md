## Strikethrough and subscript composition

```````````````````````````````` example
~sub~ ~~del~~ **bold**
.
<p><sub>sub</sub> <del>del</del> <strong>bold</strong></p>
````````````````````````````````

```````````````````````````````` example
~~a~~~b~ **c**
.
<p><del>a</del><sub>b</sub> <strong>c</strong></p>
````````````````````````````````

```````````````````````````````` example
~a **b**~
.
<p><sub>a <strong>b</strong></sub></p>
````````````````````````````````
