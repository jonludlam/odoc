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
