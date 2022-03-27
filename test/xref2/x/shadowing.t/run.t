  $ ocamlc -c -bin-annot -I . b.mli
  $ ocamlc -c -bin-annot -I . c.mli
  $ ocamlc -c -bin-annot -I . d.mli

  $ odoc compile -I . b.cmti
  Starting type_of pass
  Adding (root B).t to env
  Adding (root B).Q to env
  Adding (root B).U to env
  Adding (root B).Q.U to env
  Finished type_of pass
  Adding (root B).t to env
  Adding (root B).Q to env
  Adding (root B).U to env
  Adding (root B).Q.U to env
  $ odoc compile -I . c.cmti
  Starting type_of pass
  Adding (root C).t to env
  Adding (root C).[Q]1 to env
  Adding (root C).[U]2 to env
  Adding (root C).Q to env
  Adding (root C).U to env
  Handling include in type_of
  Removing (root C).t from env
  Removing (root C).[Q]1 from env
  Removing (root C).[U]2 from env
  Adding (root C).t to env
  Adding (root C).Q to env
  Overriding duplicate env entry: Q
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Env.add_to_elts in file "src/xref2/env.ml", line 232, characters 8-24
        Called from Odoc_xref2__Env.add_module in file "src/xref2/env.ml", line 308, characters 13-77
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Type_of.signature in file "src/xref2/type_of.ml" (inlined), line 13, characters 12-37
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
  $ odoc compile -I . d.cmti
  Starting type_of pass
  Adding (root D).[t]1 to env
  Adding (root D).Q to env
  Adding (root D).[U]3 to env
  Adding (root D).t to env
  Adding (root D).U to env
  Handling include in type_of
  Removing (root D).[t]1 from env
  Removing (root D).Q from env
  Removing (root D).[U]3 from env
  Finished handling include in type_of
  Finished type_of pass
  Adding (root D).[t]1 to env
  Adding (root D).Q to env
  Adding (root D).[U]3 to env
  Adding (root D).t to env
  Adding (root D).U to env
  Handling include of : module type of struct include unresolvedroot(C) end
  Removing (root D).[t]1 from env
  Removing (root D).Q from env
  Removing (root D).[U]3 from env
  Adding (root D).[t]1 to env
  Adding (root D).Q to env
  Adding (root D).[U]3 to env
  File "d.cmti":
  Warning: Couldn't find the following modules:
    C

