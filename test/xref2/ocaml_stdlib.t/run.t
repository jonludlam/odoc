Imitate the way the stdlib is built and how its documentation should be built.

  $ ocamlc -c -no-alias-deps -bin-annot -w -49 -o main.cmti main.mli
  $ ocamlc -c -bin-annot -I . -o main__x.cmti x.mli

'Main' doesn't depend on 'Main__x':

  $ odoc compile-deps main.cmti | grep Main__x
  [1]

  $ odoc compile --pkg ocaml -o main.odoc main.cmti -I .
  Starting type_of pass
  Adding (root Main).pervasives to env
  Adding (root Main).X to env
  Finished type_of pass
  Adding (root Main).pervasives to env
  Adding (root Main).X to env
  File "main.mli", line 3, characters 4-17:
  Warning: Canonical paths must contain a dot, eg. X.Y.
  $ odoc compile --pkg ocaml -o main__x.odoc main__x.cmti -I .
  Starting type_of pass
  Adding (root Main__x).t to env
  Finished type_of pass
  Adding (root Main__x).t to env

  $ odoc html --indent -o html main__x.odoc -I .
  $ odoc html --indent -o html main.odoc -I .
  Adding (root Main).pervasives to env
  Adding (root Main).X to env
  Adding (root Main).X.t to env

The page for Main should include the synopsis from X:

  $ cat html/ocaml/Main/index.html | grep "Synopsis"
      </div><div class="spec-doc"><p>Synopsis</p></div>
