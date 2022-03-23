  $ ocamlc -bin-annot -c a.ml
  $ ocamlc -bin-annot -c b.ml
  $ odoc compile a.cmt
  Read include - shadowed.types = 
  content=module {Bar}1
  
  Read include - shadowed.types = 
  content=module Bar
  
  Starting type_of pass
  Adding (root A).Foo to env
  Adding (root A).Baz to env
  Adding (root A).Bar to env
  Adding (root A).Baz.Bar to env
  Adding (root A).Baz.Bar to env
  Overriding duplicate env entry: Bar
  Handling include in type_of
  Removing (root A).Bar from env
  Adding (root A).Bar to env
  Adding (root A).Bar to env
  Overriding duplicate env entry: Bar
  Finished handling include in type_of
  Finished type_of pass
  Adding (root A).Foo to env
  Adding (root A).Baz to env
  Adding (root A).Bar to env
  Adding (root A).Baz.Bar to env
  Adding (root A).Baz.Bar to env
  Overriding duplicate env entry: Bar
  Handling include of : module type of struct include identifier((root A).Baz, false) end
  Removing (root A).Bar from env
  Adding (root A).Bar to env
  Adding (root A).Bar to env
  Overriding duplicate env entry: Bar
  Removing (root A).Bar from env
  Adding (root A).Bar to env
  Adding (root A).Bar to env
  Overriding duplicate env entry: Bar
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
  Adding (root B).Foo to env
  Adding (root B).Baz to env
  Adding (root B).Bar to env
  Adding (root B).Bar to env
  Overriding duplicate env entry: Bar
  Handling include in type_of
  Removing (root B).Bar from env
  Removing (root B).Bar from env
  Failed to find Bar
  odoc: internal error, uncaught exception:
        Not_found
        Raised at Odoc_xref2__Env.ElementsByName.remove in file "src/xref2/env.ml", line 122, characters 101-116
        Called from Odoc_xref2__Env.remove in file "src/xref2/env.ml", line 248, characters 11-52
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Type_of.include_ in file "src/xref2/type_of.ml", line 112, characters 14-57
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 25, characters 31-47
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Stdlib__List.map in file "list.ml", line 92, characters 32-39
        Called from Odoc_xref2__Type_of.signature_items in file "src/xref2/type_of.ml", line 20, characters 4-253
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 14, characters 2-24
        Called from Odoc_xref2__Type_of.simple_expansion in file "src/xref2/type_of.ml", line 104, characters 30-48
        Called from Odoc_xref2__Type_of.u_module_type_expr in file "src/xref2/type_of.ml", line 90, characters 45-70
        Called from Odoc_xref2__Type_of.include_ in file "src/xref2/type_of.ml", line 115, characters 33-68
        Called from Odoc_xref2__Type_of.signature_items.(fun) in file "src/xref2/type_of.ml", line 25, characters 31-47
        Called from Stdlib__List.map in file "list.ml", line 92, characters 20-23
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
  $ odoc html-generate -o html a.odoc
  $ odoc support-files -o html
  $ rsync -avz html /tmp/html/
  building file list ... done
  html/
  html/highlight.pack.js
  html/odoc.css
  html/A/
  html/A/index.html
  html/A/Baz/
  html/A/Baz/index.html
  html/A/Foo/
  html/A/Foo/index.html
  
  sent 12311 bytes  received 154 bytes  24930.00 bytes/sec
  total size is 32216  speedup is 2.58

