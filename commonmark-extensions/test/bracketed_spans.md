Bracketed spans work like links with attributes, but
without the link destination.

```````````````````````````````` example
[foo]{#ident .class key="value value" key2=value2}
.
<p><span id="ident" class="class" data-key="value value" data-key2="value2">foo</span></p>
````````````````````````````````

```````````````````````````````` example
[foo
*bar*]{#ident
.class}
.
<p><span id="ident" class="class">foo
<em>bar</em></span></p>
````````````````````````````````

An attribute is required:

```````````````````````````````` example
[foo]
.
<p>[foo]</p>
````````````````````````````````
