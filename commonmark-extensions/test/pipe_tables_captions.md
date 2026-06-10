
Basic Match
=================

```````````````````````````````` example
Test|Table
----|-----
Test|row
: Test Caption

Test ending
.
<table>
<caption>Test Caption</caption>
<thead>
<tr>
<th>Test</th>
<th>Table</th>
</tr>
</thead>
<tbody>
<tr>
<td>Test</td>
<td>row</td>
</tr>
</tbody>
</table>
<p>Test ending</p>
````````````````````````````````

If the colon is placed in the middle, it will work:

```````````````````````````````` example
Test|Table
----|-----
Test|row
: Test Caption
Test|row2

Test ending
.
<table>
<caption>Test Caption</caption>
<thead>
<tr>
<th>Test</th>
<th>Table</th>
</tr>
</thead>
<tbody>
<tr>
<td>Test</td>
<td>row</td>
</tr>
<tr>
<td>Test</td>
<td>row2</td>
</tr>
</tbody>
</table>
<p>Test ending</p>
````````````````````````````````

Before the sign that there's a problem, there are signs.

```````````````````````````````` example
: Inappropriate

Test|Table
----|-----
Test|row
Test|row2

Test ending
.
<p>: Inappropriate</p>
<table>
<thead>
<tr>
<th>Test</th>
<th>Table</th>
</tr>
</thead>
<tbody>
<tr>
<td>Test</td>
<td>row</td>
</tr>
<tr>
<td>Test</td>
<td>row2</td>
</tr>
</tbody>
</table>
<p>Test ending</p>
````````````````````````````````



Test with quote
===============

```````````````````````````````` example_table_captions
> Test  | Table
> ------|------
> Row 1 | Every
> Row 2 | Day
>: Test Caption
>
> Paragraph
.
<blockquote>
<table>
<caption>Test Caption</caption>
<thead>
<tr>
<th>Test</th>
<th>Table</th>
</tr>
</thead>
<tbody>
<tr>
<td>Row 1</td>
<td>Every</td>
</tr>
<tr>
<td>Row 2</td>
<td>Day</td>
</tr>
</tbody>
</table>
<p>Paragraph</p>
</blockquote>
````````````````````````````````


Test with list
==============

```````````````````````````````` example_table_captions
 1. First entry
 2. Second entry

    Col 1|Col 2
    -|-
    Row 1|Part 2
    Row 2|Part 2
    : Test Caption
.
<ol>
<li>
<p>First entry</p>
</li>
<li>
<p>Second entry</p>
<table>
<caption>Test Caption</caption>
<thead>
<tr>
<th>Col 1</th>
<th>Col 2</th>
</tr>
</thead>
<tbody>
<tr>
<td>Row 1</td>
<td>Part 2</td>
</tr>
<tr>
<td>Row 2</td>
<td>Part 2</td>
</tr>
</tbody>
</table>
</li>
</ol>
````````````````````````````````


Table with UTF-8
================

Basic example.

```````````````````````````````` example_table_captions
|Col 1|Col 2|
|-----|-----|
|✓    |✓    |
|✓    |✓    |
: Done ✓
.
<table>
<caption>Done ✓</caption>
<thead>
<tr>
<th>Col 1</th>
<th>Col 2</th>
</tr>
</thead>
<tbody>
<tr>
<td>✓</td>
<td>✓</td>
</tr>
<tr>
<td>✓</td>
<td>✓</td>
</tr>
</tbody>
</table>
````````````````````````````````

Hiragana-containing table.

```````````````````````````````` example_table_captions
|ぁ|ぃ|
|-|-|
|ぃ|ぃ|
: ぁ
.
<table>
<caption>ぁ</caption>
<thead>
<tr>
<th>ぁ</th>
<th>ぃ</th>
</tr>
</thead>
<tbody>
<tr>
<td>ぃ</td>
<td>ぃ</td>
</tr>
</tbody>
</table>
````````````````````````````````

Test russian symbols.

```````````````````````````````` example_table_captions
|Колонка 1|Колонка 2|
|---------|---------|
|Ячейка 1 |Ячейка 2 |
: Подпись
.
<table>
<caption>Подпись</caption>
<thead>
<tr>
<th>Колонка 1</th>
<th>Колонка 2</th>
</tr>
</thead>
<tbody>
<tr
<td>Ячейка 1</td>
<td>Ячейка 2</td>
</tr>
</tbody>
</table>
````````````````````````````````
