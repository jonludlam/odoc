Two extensions cannot handle the same code blocks.

  $ odoc_clash --version
  Extensions one and two both handle code blocks in shout.
  [2]

An extension cannot handle the raw markup for the targets that odoc renders
itself, or replace odoc's own support files.

  $ ODOC_CLASH=raw-markup odoc_clash --version
  Extension one cannot handle the raw markup for html.
  [2]
  $ ODOC_CLASH=support-file odoc_clash --version
  Extension one cannot replace odoc's support file odoc.css.
  [2]

Nor can it handle tags that would not be parsed as custom tags: odoc's own, such
as @see, which is how @see-also is read, or the variants of a prefix.

  $ ODOC_CLASH=builtin-tag odoc_clash --version
  Extension one cannot handle the tags see-also.
  [2]
  $ ODOC_CLASH=dotted-tag odoc_clash --version
  Extension one cannot handle the tags admonition.note.
  [2]
