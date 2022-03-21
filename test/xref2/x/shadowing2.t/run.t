  $ ocamlc -c -bin-annot a.ml
  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml
  $ odoc compile a.cmt
  $ odoc compile -I . b.cmt
  $ odoc compile -I . c.cmt
  $ odoc_print b.odoc

  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc link -I . c.odoc
  $ odoc html-generate -o html a.odocl
  $ odoc html-generate -o html b.odocl
  $ odoc html-generate -o html c.odocl
  $ odoc support-files -o html
  $ rsync -avz html /tmp/html
