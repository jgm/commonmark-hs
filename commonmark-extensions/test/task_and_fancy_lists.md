## Task lists with fancy ordered lists

```````````````````````````````` example
- [ ] an unchecked task list item
- [x] checked item

A.  upper alpha
B.  upper alpha

- ordinary bullet
.
<ul class="task-list">
<li><input type="checkbox" disabled="" />an unchecked task list item</li>
<li><input type="checkbox" disabled="" checked="" />checked item</li>
</ul>
<ol type="A">
<li>upper alpha</li>
<li>upper alpha</li>
</ol>
<ul>
<li>ordinary bullet</li>
</ul>
````````````````````````````````

```````````````````````````````` example
* [ ] an unchecked task list item

  with two paragraphs

* [x] checked item

i. Lower Roman
ii. Lower Roman
.
<ul class="task-list">
<li><input type="checkbox" disabled="" /><p>an unchecked task list item</p>
<p>with two paragraphs</p>
</li>
<li><input type="checkbox" disabled="" checked="" /><p>checked item</p>
</li>
</ul>
<ol type="i">
<li>Lower Roman</li>
<li>Lower Roman</li>
</ol>
````````````````````````````````

```````````````````````````````` example
- [ ] outer
  - [x] inner
.
<ul class="task-list">
<li><input type="checkbox" disabled="" />outer
<ul class="task-list">
<li><input type="checkbox" disabled="" checked="" />inner</li>
</ul></li>
</ul>
````````````````````````````````
