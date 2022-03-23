We don't have to provide parents to modules now

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  Starting type_of pass
  Adding (root Test).t to env
  Adding (root Test).M to env
  Adding (root Test).M.N.u to env
  Finished type_of pass
  Adding (root Test).t to env
  Adding (root Test).M to env
  Adding (root Test).M.N.u to env
  Adding (root Test).M.N.u to env
  $ odoc link test.odoc
  Adding (root Test).t to env
  Adding (root Test).M to env
  Adding (root Test).M.N.u to env
  $ odoc html-generate test.odocl -o output --flat
  $ odoc latex-generate test.odocl -o output
  $ odoc man-generate test.odocl -o output

  $ find output | sort
  output
  output/Test-M-module-type-N.html
  output/Test-M.html
  output/Test.3o
  output/Test.M.3o
  output/Test.html
  output/Test.tex

  $ find output -mindepth 1 | sort
  output/Test-M-module-type-N.html
  output/Test-M.html
  output/Test.3o
  output/Test.M.3o
  output/Test.html
  output/Test.tex

