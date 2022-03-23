  $ ocamlc -c -bin-annot Bar.mli
  $ ocamlc -c -bin-annot Baz.mli
  $ ocamlc -c -bin-annot foo.mli
  $ ocamlc -c -bin-annot moo.mli

  $ odoc compile page.mld --child bar --child module-baz --child Foo --child module-Moo
  $ odoc compile Bar.cmti -I . --parent page
  Starting type_of pass
  Adding (root Bar).t to env
  Finished type_of pass
  Adding (root Bar).t to env
  $ odoc compile Baz.cmti -I . --parent page-page
  Starting type_of pass
  Adding (root Baz).t to env
  Finished type_of pass
  Adding (root Baz).t to env
  $ odoc compile foo.cmti -I . --parent page
  Starting type_of pass
  Adding (root Foo).t to env
  Finished type_of pass
  Adding (root Foo).t to env
  $ odoc compile moo.cmti -I . --parent page-page
  Starting type_of pass
  Adding (root Moo).t to env
  Finished type_of pass
  Adding (root Moo).t to env

  $ odoc link page-page.odoc -I .
  $ odoc link Bar.odoc -I .
  Adding (root Bar).t to env
  $ odoc link Baz.odoc -I .
  Adding (root Baz).t to env
  $ odoc link foo.odoc -I .
  Adding (root Foo).t to env
  $ odoc link moo.odoc -I .
  Adding (root Moo).t to env

  $ for i in *.odocl; do odoc html-generate $i -o html; done
  $ odoc support-files -o html
