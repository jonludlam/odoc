The definition behind a name in an implementation can be in another library.
Library [b] calls [A.f] from library [lib]. A library [other] also has a module
[A], which [b] was not compiled against.

  $ (cd lib && ocamlc -c -bin-annot a.ml)
  $ (cd other && ocamlc -c -bin-annot a.ml)
  $ (cd b && ocamlc -c -bin-annot -I ../lib b.ml)
  $ odoc compile-impl lib/a.cmt --output-dir h --parent-id lib --source-id lib/a.ml -L lib:h/lib
  $ odoc compile-impl other/a.cmt --output-dir h --parent-id other --source-id other/a.ml -L other:h/other
  $ odoc compile-impl b/b.cmt --output-dir h --parent-id b --source-id b/b.ml -L b:h/b -L lib:h/lib

The libraries are given with [-L] alone. A search that ignores the libraries
[b] recorded finds the [A] of [other].

  $ odoc link h/b/impl-b.odoc -L other:h/other -L b:h/b -L lib:h/lib
  $ odoc html-generate-source --impl h/b/impl-b.odocl -o html b/b.ml
  $ grep -o 'href="[^"#]*/a.ml.html[^"]*"' html/b/b.ml.html
  href="../lib/a.ml.html#val-f"
