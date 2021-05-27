## Rebase relative paths

This extension causes relative paths in images and links to be
rebased to the (relative) directory of the containing file.
For example, if the source file is `foo/test.md` and contains
an image `![image](img.jpg)`, the image path will be rewritten
to `foo/img.jpg`.  Absolute paths, absolute URLs, empty paths,
and paths that are purely fragments (i.e., start with `#`) are
left unchanged.

This feature facilitates converting documents that are split
into multiple files in different directories.

