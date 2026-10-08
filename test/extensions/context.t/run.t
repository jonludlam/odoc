A handler is given a context, the page it renders in.

Its warnings are odoc's warnings, at the element it renders, and are errors with
--warn-error.

  $ cat > index.mld << EOF
  > {0 Context}
  > 
  > {@shout[123]}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc index.mld
  $ odoc_shout link _odoc/pkg/page-index.odoc
  $ odoc_shout html-generate -o html _odoc/pkg/page-index.odocl
  File "index.mld", line 3, characters 0-13:
  Warning: Nothing to shout.
  $ odoc_shout html-generate --warn-error -o html _odoc/pkg/page-index.odocl
  File "index.mld", line 3, characters 0-13:
  Error: Nothing to shout.
  ERROR: Warnings have been generated.
  [1]

The search index renders the element again, outside its page, and does not
report the warning a second time.

  $ odoc_shout compile-index --warn-error -o index.odoc-index _odoc/pkg/page-index.odocl

A handler knows which output it renders for.

  $ cat > backend.mld << EOF
  > {0 Backend}
  > 
  > {@shout backend[hello]}
  > EOF
  $ odoc_shout compile --parent-id pkg --output-dir _odoc backend.mld
  $ odoc_shout link _odoc/pkg/page-backend.odoc
  $ odoc_shout html-generate -o html _odoc/pkg/page-backend.odocl
  $ grep -o 'HELLO in [a-z]*' html/pkg/backend.html
  HELLO in html
  $ odoc_shout latex-generate -o latex _odoc/pkg/page-backend.odocl
  $ grep -o 'HELLO in [a-z]*' latex/pkg/backend.tex
  HELLO in latex
  $ odoc_shout man-generate -o man _odoc/pkg/page-backend.odocl
  $ grep -o 'HELLO in [a-z]*' man/pkg/backend.3o
  HELLO in man
  $ odoc_shout markdown-generate -o markdown _odoc/pkg/page-backend.odocl
  $ grep -o 'HELLO in [a-z]*' markdown/pkg/backend.md
  HELLO in markdown

What it requires is noted for the page it renders in: here the page of the
submodule, not the page of the module.

  $ cat > n.mli << EOF
  > module N : sig
  >   (** {@shout[only in the submodule]} *)
  > end
  > EOF
  $ ocamlc -c -bin-annot n.mli
  $ odoc_shout compile --parent-id pkg --output-dir _odoc n.cmti
  $ odoc_shout link _odoc/pkg/n.odoc
  $ odoc_shout html-generate -o html _odoc/pkg/n.odocl
  $ grep -c '<style>' html/pkg/N/index.html html/pkg/N/N/index.html
  html/pkg/N/index.html:0
  html/pkg/N/N/index.html:1
