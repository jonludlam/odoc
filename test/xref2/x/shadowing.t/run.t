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
  Adding (root C).{Q}1 to env
  Adding (root C).{U}2 to env
  Adding (root C).Q to env
  Adding (root C).U to env
  Handling include in type_of
  Removing (root C).t from env
  Removing (root C).{Q}1 from env
  Removing (root C).{U}2 from env
  Adding (root C).t to env
  Adding (root C).Q to env
  Overriding duplicate env entry: Q
  Adding (root C).U to env
  Overriding duplicate env entry: U
  Finished handling include in type_of
  Adding (root C).Q.{U}5 to env
  Adding (root C).Q.U to env
  Handling include in type_of
  Removing (root C).Q.{U}5 from env
  Adding (root C).Q.U to env
  Overriding duplicate env entry: U
  Finished handling include in type_of
  Finished type_of pass
  Adding (root C).t to env
  Adding (root C).{Q}1 to env
  Adding (root C).{U}2 to env
  Adding (root C).Q to env
  Adding (root C).U to env
  Handling include of : module type of struct include unresolvedroot(B) end
  Removing (root C).t from env
  Removing (root C).{Q}1 from env
  Removing (root C).{U}2 from env
  Adding (root C).t to env
  Adding (root C).{Q}8 to env
  Adding (root C).{U}9 to env
  Removing (root C).t from env
  Removing (root C).{Q}1 from env
  Removing (root C).{U}2 from env
  Adding (root C).t to env
  Adding (root C).{Q}8 to env
  Adding (root C).{U}9 to env
  Adding (root C).Q.{U}5 to env
  Adding (root C).Q.U to env
  Handling include of : module type of struct include unresolvedroot(B).Q end
  Removing (root C).Q.{U}5 from env
  Adding (root C).Q.{U}12 to env
  Removing (root C).Q.{U}5 from env
  Adding (root C).Q.{U}12 to env
  $ odoc compile -I . d.cmti
  Starting type_of pass
  Adding (root D).{t}1 to env
  Adding (root D).Q to env
  Adding (root D).{U}6 to env
  Adding (root D).t to env
  Adding (root D).U to env
  Handling include in type_of
  Removing (root D).{t}1 from env
  Removing (root D).Q from env
  Removing (root D).{U}6 from env
  Adding (root D).t to env
  Overriding duplicate env entry: t
  Adding (root D).{Q}8 to env
  Adding (root D).{U}9 to env
  Adding (root D).Q to env
  Adding (root D).U to env
  Overriding duplicate env entry: U
  Handling include in type_of
  Removing (root D).t from env
  Removing (root D).{Q}8 from env
  Removing (root D).{U}9 from env
  Adding (root D).t to env
  Adding (root D).Q to env
  Overriding duplicate env entry: Q
  Adding (root D).U to env
  Overriding duplicate env entry: U
  Finished handling include in type_of
  Finished handling include in type_of
  Finished type_of pass
  Adding (root D).{t}1 to env
  Adding (root D).Q to env
  Adding (root D).{U}6 to env
  Adding (root D).t to env
  Adding (root D).U to env
  Handling include of : module type of struct include unresolvedroot(C) end
  Removing (root D).{t}1 from env
  Removing (root D).Q from env
  Removing (root D).{U}6 from env
  Adding (root D).{t}10 to env
  Adding (root D).{Q}8 to env
  Adding (root D).{U}9 to env
  Adding (root D).Q to env
  Adding (root D).{U}9 to env
  Overriding duplicate env entry: {U}9
  Handling include of : module type of struct include r((root B)) end
  Removing (root D).{t}10 from env
  Removing (root D).{Q}8 from env
  Removing (root D).{U}9 from env
  Adding (root D).{t}15 to env
  Adding (root D).{Q}23 to env
  Adding (root D).{U}16 to env
  Removing (root D).{t}10 from env
  Removing (root D).{Q}8 from env
  Removing (root D).{U}9 from env
  Adding (root D).{t}15 to env
  Adding (root D).{Q}23 to env
  Adding (root D).{U}16 to env
  Failed to find {U}9
  odoc: internal error, uncaught exception:
        Not_found
        Raised at Odoc_xref2__Env.ElementsByName.remove in file "src/xref2/env.ml", line 122, characters 101-116
        Called from Odoc_xref2__Env.remove in file "src/xref2/env.ml", line 248, characters 11-52
        Called from Odoc_xref2__Env.update_module in file "src/xref2/env.ml", line 312, characters 2-23
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 216, characters 16-122
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 350, characters 10-45
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 246, characters 21-35
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 67, characters 15-54
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

