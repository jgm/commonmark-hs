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
