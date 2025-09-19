  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate -o html test.odocl
  $ odoc support-files -o html

Enable the next line for easy debugging

$ rsync -avz html /tmp/subst_problem/

