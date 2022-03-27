  $ ocamlc -bin-annot -c a.ml
  $ ocamlc -bin-annot -c b.ml
  $ odoc compile a.cmt
  Read include - shadowed.types = 
  content=module [Bar]1
  
  Read include - shadowed.types = 
  content=module Bar
  
  Starting type_of pass
  Adding (root A).Foo to env
  Adding (root A).Baz to env
  Adding (root A).Bar to env
  Adding (root A).Baz.Bar to env
  Adding (root A).Baz.Bar to env
  Overriding duplicate env entry: Bar
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Env.add_to_elts in file "src/xref2/env.ml", line 232, characters 8-24
        Called from Odoc_xref2__Env.add_module in file "src/xref2/env.ml", line 308, characters 13-77
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 13, characters 12-37
        Called from Odoc_xref2__Type_of.module_type_expr in file "src/xref2/type_of.ml", line 67, characters 30-48
        Called from Odoc_xref2__Type_of.module_ in file "src/xref2/type_of.ml", line 37, characters 27-79
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 23, characters 38-51
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Odoc_xref2__Type_of.signature_items in file "src/xref2/type_of.ml", line 20, characters 4-253
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 14, characters 2-24
        Called from Odoc_xref2__Type_of.signature.loop in file "src/xref2/type_of.ml", line 128, characters 14-30
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 65, characters 15-38
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 58, characters 21-47
        Called from Odoc_xref2__Lookup_failures.with_ref in file "src/xref2/lookup_failures.ml", line 13, characters 10-14
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 60, characters 20-37
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 93, characters 4-49
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 52, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 87, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 65, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 70, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 226, characters 6-136
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]
  $ odoc compile -I . b.cmt
  Read include - shadowed.types = 
  content=module Foo
  module Baz
  module Bar
  
  Starting type_of pass
  Adding (root B).Foo to env
  Adding (root B).Baz to env
  Adding (root B).Bar to env
  Handling include in type_of
  Removing (root B).Foo from env
  Removing (root B).Baz from env
  Removing (root B).Bar from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root B).Foo to env
  Adding (root B).Baz to env
  Adding (root B).Bar to env
  Handling include of : module type of struct include unresolvedroot(A) end
  Removing (root B).Foo from env
  Removing (root B).Baz from env
  Removing (root B).Bar from env
  Adding (root B).Foo to env
  Adding (root B).Baz to env
  Adding (root B).Bar to env
  File "b.cmt":
  Warning: Couldn't find the following modules:
    A
  $ odoc html-generate -o html a.odoc
  odoc: file.odoc argument: no `a.odoc' file or directory
  Usage: odoc html-generate [OPTION]... file.odoc
  Try `odoc html-generate --help' or `odoc --help' for more information.
  [2]
  $ odoc support-files -o html
  $ rsync -avz html /tmp/html/
  building file list ... done
  html/
  html/highlight.pack.js
  html/odoc.css
  
  sent 10638 bytes  received 70 bytes  21416.00 bytes/sec
  total size is 28403  speedup is 2.65

