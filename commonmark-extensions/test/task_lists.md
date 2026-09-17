## Task lists

As in GitHub-flavored Markdown.

```````````````````````````````` example
- [ ] an unchecked task list item
- [x] checked item
.
<ul class="task-list">
<li><input type="checkbox" disabled="" />an unchecked task list item</li>
<li><input type="checkbox" disabled="" checked="" />checked item</li>
</ul>
````````````````````````````````

```````````````````````````````` example
* [ ] an unchecked task list item

  with two paragraphs

* [x] checked item
.
<ul class="task-list">
<li><input type="checkbox" disabled="" /><p>an unchecked task list item</p>
<p>with two paragraphs</p>
</li>
<li><input type="checkbox" disabled="" checked="" /><p>checked item</p>
</li>
</ul>
````````````````````````````````


```````````````````````````````` example
- [x]unreal
.
<ul>
<li>[x]unreal</li>
</ul>
````````````````````````````````


```````````````````````````````` example
-  [x] real

  not indented enough
.
<ul class="task-list">
<li><input type="checkbox" disabled="" checked="" />real</li>
</ul>
<p>not indented enough</p>
````````````````````````````````


```````````````````````````````` example
- [x] * some text
- [ ] > some text
- [x]
  * some text
- [ ]
  > some text
.
<ul class="task-list">
<li><input type="checkbox" disabled="" checked="" />* some text</li>
<li><input type="checkbox" disabled="" />&gt; some text</li>
<li><input type="checkbox" disabled="" checked="" /><ul>
<li>some text</li>
</ul></li>
<li><input type="checkbox" disabled="" /><blockquote>
<p>some text</p>
</blockquote></li>
</ul>
````````````````````````````````

There is no empty paragraph after the `]`.

```````````````````````````````` example
- [x] * some text

- [x]

  some text

- [x]→

  some text
.
<ul class="task-list">
<li><input type="checkbox" disabled="" checked="" /><p>* some text</p></li>
<li><input type="checkbox" disabled="" checked="" /><p>some text</p>
</li>
<li><input type="checkbox" disabled="" checked="" /><p>some text</p>
</li>
</ul>
````````````````````````````````

```````````````````````````````` example
- [ ]
- [ ] b
.
<ul class="task-list">
<li><input type="checkbox" disabled="" /></li>
<li><input type="checkbox" disabled="" />b</li>
</ul>
````````````````````````````````

Tight/loose classification should match plain lists.  Blank lines
after a list don't make it loose:

```````````````````````````````` example
- [ ] a
- [ ] b


x
.
<ul class="task-list">
<li><input type="checkbox" disabled="" />a</li>
<li><input type="checkbox" disabled="" />b</li>
</ul>
<p>x</p>
````````````````````````````````

A blank line after a nested task list makes the outer list loose,
just as with plain lists:

```````````````````````````````` example
- [ ] a
  - [ ] b

- [ ] c
.
<ul class="task-list">
<li>
<input type="checkbox" disabled="" /><p>a</p>
<ul class="task-list">
<li><input type="checkbox" disabled="" />b</li>
</ul>
</li>
<li>
<input type="checkbox" disabled="" /><p>c</p>
</li>
</ul>
````````````````````````````````
