  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ ocamlc -i c.mli

  $ odoc compile a.cmti
  $ odoc compile b.cmti
  $ odoc compile -I . c.cmti
 
  $ odoc_print --short c.odoc 
