Build the documentation of a simple Dune library.

  $ dune build @install @doc
  Error: Unresolved path doesn't look like unresolve_resolve_module_path: unresolvedroot(Dune_odoc_test).Foo != identifier((root Dune_odoc_test), false).Foo

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Bar/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Foo/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/index.html
  _build/default/_doc/_html/dune_odoc_test/index.html
  _build/default/_doc/_html/index.html
