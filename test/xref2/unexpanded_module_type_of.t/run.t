Unexpanded `module type of`
===========================

This is a test for [this issue](https://github.com/ocaml/odoc/issues/500)

  $ cat test0.mli
  type t

  $ cat test.mli
  module M: sig include module type of Test0 end
  
  $ ocamlc -c -bin-annot test0.mli
  $ ocamlc -c -bin-annot test.mli

Compiling an odoc file for `test` without compiling one for `test0` 
should _not_ result in an exception, merely a warning.

  $ odoc compile --package test test.cmti
  Starting type_of pass
  Adding (root Test).M to env
  Adding (root Test).M.t to env
  Handling include in type_of
  Removing (root Test).M.t from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root Test).M to env
  Adding (root Test).M.t to env
  Handling include of : module type of unresolvedroot(Test0)
  Removing (root Test).M.t from env
  Adding (root Test).M.t to env
  File "test.cmti":
  Warning: Couldn't find the following modules:
    Test0
