## Tables (extension)

GFM enables the `table` extension, where an additional leaf block type is
available.

A [table](@) is an arrangement of data with rows and columns, consisting of a
single header row, a [delimiter row] separating the header from the data, and
zero or more data rows.

Each row consists of cells containing arbitrary text, in which [inlines] are
parsed, separated by pipes (`|`).  A leading and trailing pipe is also
recommended for clarity of reading, and if there's otherwise parsing ambiguity.
Spaces between pipes and cell content are trimmed.  Block-level elements cannot
be inserted in a table.

The [delimiter row](@) consists of cells whose only content are hyphens (`-`),
and optionally, a leading or trailing colon (`:`), or both, to indicate left,
right, or center alignment respectively.

```````````````````````````````` example
| foo | bar |
| --- | --- |
| baz | bim |
.
<table>
<thead>
<tr>
<th>foo</th>
<th>bar</th>
</tr>
</thead>
<tbody>
<tr>
<td>baz</td>
<td>bim</td>
</tr>
</tbody>
</table>
````````````````````````````````

Cells in one column don't need to match length, though it's easier to read if
they are. Likewise, use of leading and trailing pipes may be inconsistent:

```````````````````````````````` example
| abc | defghi |
:-: | -----------:
bar | baz
.
<table>
<thead>
<tr>
<th style="text-align: center;">abc</th>
<th style="text-align: right;">defghi</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: center;">bar</td>
<td style="text-align: right;">baz</td>
</tr>
</tbody>
</table>
````````````````````````````````

Include a pipe in a cell's content by escaping it, including inside other
inline spans:

```````````````````````````````` example
| f\|oo  |
| ------ |
| b `\|` az |
| b **\|** im |
.
<table>
<thead>
<tr>
<th>f|oo</th>
</tr>
</thead>
<tbody>
<tr>
<td>b <code>|</code> az</td>
</tr>
<tr>
<td>b <strong>|</strong> im</td>
</tr>
</tbody>
</table>
````````````````````````````````

The table is broken at the first line not containing an
unescaped `|`:

```````````````````````````````` example
| abc | def |
| --- | --- |
| bar | baz |
> bar
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
<blockquote>
<p>bar</p>
</blockquote>
````````````````````````````````

```````````````````````````````` example
| abc | def |
| --- | --- |
| bar | baz |

bar
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
<p>bar</p>
````````````````````````````````

```````````````````````````````` example
| abc | def |
| --- | --- |
| bar | baz |
bar
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
<p>bar</p>
````````````````````````````````

The header row must match the [delimiter row] in the number of cells.  If not,
a table will not be recognized:

```````````````````````````````` example
| abc | def |
| --- |
| bar |
.
<p>| abc | def |
| --- |
| bar |</p>
````````````````````````````````

The remainder of the table's rows may vary in the number of cells.  If there
are a number of cells fewer than the number of cells in the header row, empty
cells are inserted.  If there are greater, the excess is ignored:

```````````````````````````````` example
| abc | def |
| --- | --- |
| bar |
| bar | baz | boo |
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
<tbody>
<tr>
<td>bar</td>
<td></td>
</tr>
<tr>
<td>bar</td>
<td>baz</td>
</tr>
</tbody>
</table>
````````````````````````````````

If there are no rows in the body, no `<tbody>` is generated in HTML output:

```````````````````````````````` example
| abc | def |
| --- | --- |
.
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
</table>
````````````````````````````````

Here are some non-tables:

```````````````````````````````` example
| Not enough table | to be considered table |

| Not enough table | to be considered table |
| Not enough table | to be considered table |

| ---- | --- |
.
<p>| Not enough table | to be considered table |</p>
<p>| Not enough table | to be considered table |
| Not enough table | to be considered table |</p>
<p>| ---- | --- |</p>
````````````````````````````````

A table may be indented up to three spaces:

```````````````````````````````` example
   a | b | c
   - | - | -
.
<table>
<thead>
<tr>
<th>a</th>
<th>b</th>
<th>c</th>
</tr>
</thead>
</table>
````````````````````````````````

```````````````````````````````` example
    a | b | c
    - | - | -
.
<pre><code>a | b | c
- | - | -
</code></pre>
````````````````````````````````


Pipe tables have exactly one header row, and do not interrupt paragraphs.

```````````````````````````````` example
| Too much table | to be considered table |
| Too much table | to be considered table |
|----------------|------------------------|
| Too much table | to be considered table |
.
<p>| Too much table | to be considered table |
| Too much table | to be considered table |
|----------------|------------------------|
| Too much table | to be considered table |</p>
````````````````````````````````


Other block structures, like headers, have higher priority than tables.
Tables can be nested in other elements, but don't benefit from laziness.

```````````````````````````````` example
# abc | def
------|-----


> abc | def
> ----|-----


> abc | def
----|-----
.
<h1>abc | def</h1>
<p>------|-----</p>
<blockquote>
<table>
<thead>
<tr>
<th>abc</th>
<th>def</th>
</tr>
</thead>
</table>
</blockquote>
<blockquote>
<p>abc | def
----|-----</p>
</blockquote>
````````````````````````````````


As a special case, pipes in inline code in tables are escaped
with backslashes.

The parsing rule for CommonMark is that block structures are parsed before
inline structures are. Normally, this means backslashes aren't allowed to have
any effect on block structures at all. Tables do consider backslashes, but
not the same way inline syntax does.

The table parser, which runs as part of the block structure parsing stage, splits rows into cells on unescaped `|` symbols, and it replaces `\|` with a literal `|`. Then it passes the contents of each cell separately to the inline parser. This means backslash escaping pipes works even in code spans (where backslashes are usually treated as literal text), but it can also result in some counterintuitive results.

For example, consider these two rows, excerpted from the below test case:

    | Wait, what? |          \|
    | Wait, what? |         \\|

The table parser only pays attention to the backslash that is immediately followed by a pipe. This means it sees the second row as this:

    | Wait, what? |         \\|
                            -^^ escaped pipe found here
                            |
                            literal backslash with no special block-level semantics

And the inline parser is given the first backslash, followed by the pipe:

    \|

The inline parser then sees it as an backslash-escaped pipe, and writes the `|` on its own. Both rows result in exactly the same HTML.


    <tr>
    <td>Wait, what?</td>
    <td>|</td>
    </tr>
    <tr>
    <td>Wait, what?</td>
    <td>|</td>
    </tr>

You need to write `\\\|` to actually get `\|` to show up in the output.

This rule should be identical to GitHub's. See
<https://gist.github.com/notriddle/c027512ee849f12098fec3a3256c89d3>
for what they do.

This changes the behavior from what older versions of commonmark-extensions do,
but it fixes some expressiveness holes that hit older versions.

```````````````````````````````` example
| Description | Test case |
|-------------|-----------|
| Single      | `\`       |
| Double      | `\\`      |
| Basic test  | `\|`      |
| Basic test 2| `\|\|\`   |
| Basic test 3| `x\|y\|z\`|
| Not pipe    | `\.`      |
| Combo       | `\.\|\`   |
| Extra       | `\\\.`    |
| Wait, what? | `\\|`     |
| Wait, what? | `\\\|`    |
| Wait, what? | `\\\\|`   |
| Wait, what? | `\\\\\|`  |
| Wait, what? |          \|
| Wait, what? |         \\|
| Wait, what? |        \\\|
| Wait, what?x|          \|x
| Wait, what?x|         \\|x
| Wait, what?x|        \\\|x
.
<table>
<thead>
<tr>
<th>Description</th>
<th>Test case</th>
</tr>
</thead>
<tbody>
<tr>
<td>Single</td>
<td><code>\</code></td>
</tr>
<tr>
<td>Double</td>
<td><code>\\</code></td>
</tr>
<tr>
<td>Basic test</td>
<td><code>|</code></td>
</tr>
<tr>
<td>Basic test 2</td>
<td><code>||\</code></td>
</tr>
<tr>
<td>Basic test 3</td>
<td><code>x|y|z\</code></td>
</tr>
<tr>
<td>Not pipe</td>
<td><code>\.</code></td>
</tr>
<tr>
<td>Combo</td>
<td><code>\.|\</code></td>
</tr>
<tr>
<td>Extra</td>
<td><code>\\\.</code></td>
</tr>
<tr>
<td>Wait, what?</td>
<td><code>\|</code></td>
</tr>
<tr>
<td>Wait, what?</td>
<td><code>\\|</code></td>
</tr>
<tr>
<td>Wait, what?</td>
<td><code>\\\|</code></td>
</tr>
<tr>
<td>Wait, what?</td>
<td><code>\\\\|</code></td>
</tr>
<tr>
<td>Wait, what?</td>
<td>|</td>
</tr>
<tr>
<td>Wait, what?</td>
<td>|</td>
</tr>
<tr>
<td>Wait, what?</td>
<td>\|</td>
</tr>
<tr>
<td>Wait, what?x</td>
<td>|x</td>
</tr>
<tr>
<td>Wait, what?x</td>
<td>|x</td>
</tr>
<tr>
<td>Wait, what?x</td>
<td>\|x</td>
</tr>
</tbody>
</table>
````````````````````````````````
