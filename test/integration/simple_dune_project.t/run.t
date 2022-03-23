Build the documentation of a simple Dune library.

  $ dune build @install @doc
  Starting type_of pass
  Adding (root Dune_odoc_test).Bar to env
  Adding (root Dune_odoc_test).Foo to env
  Finished type_of pass
  Adding (root Dune_odoc_test).Bar to env
  Adding (root Dune_odoc_test).Foo to env
  Starting type_of pass
  Adding (root Dune_odoc_test__Foo).t to env
  Finished type_of pass
  Adding (root Dune_odoc_test__Foo).t to env
  Starting type_of pass
  Adding (root Dune_odoc_test__Bar).t to env
  Finished type_of pass
  Adding (root Dune_odoc_test__Bar).t to env
  Adding (root Dune_odoc_test).Bar to env
  Adding (root Dune_odoc_test).Foo to env
  Adding (root Dune_odoc_test).Bar.t to env
  Adding (root Dune_odoc_test).Foo.t to env

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Bar/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Foo/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/index.html
  _build/default/_doc/_html/dune_odoc_test/index.html
  _build/default/_doc/_html/index.html
