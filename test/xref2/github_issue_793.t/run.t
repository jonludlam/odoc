This test checks for handling of type substitutions.
Specifically, there was an issue where the code in this test
caused an exception during compilation. The absence of the
exception shows this working correctly.

  $ ocamlc -c irmin_layers_intf.ml -bin-annot -I .
  $ odoc compile -I . irmin_layers_intf.cmt
  Starting type_of pass
  Adding (root Irmin_layers_intf).Foo to env
  Adding (root Irmin_layers_intf).A.unrelated to env
  Adding (root Irmin_layers_intf).B.unrelated to env
  Adding (root Irmin_layers_intf).B.conflicting_type to env
  Adding (root Irmin_layers_intf).B.Conflicting_module to env
  Handling include in type_of
  Removing (root Irmin_layers_intf).B.unrelated from env
  Finished handling include in type_of
  Adding (root Irmin_layers_intf).C.unrelated to env
  Adding (root Irmin_layers_intf).C.conflicting_type to env
  Adding (root Irmin_layers_intf).C.Conflicting_module to env
  Handling include in type_of
  Removing (root Irmin_layers_intf).C.unrelated from env
  Removing (root Irmin_layers_intf).C.conflicting_type from env
  Removing (root Irmin_layers_intf).C.Conflicting_module from env
  Finished handling include in type_of
  Adding (root Irmin_layers_intf).Maker.hash to env
  Adding (root Irmin_layers_intf).Maker.Make to env
  Finished type_of pass
  Adding (root Irmin_layers_intf).Foo to env
  Adding (root Irmin_layers_intf).A.unrelated to env
  Adding (root Irmin_layers_intf).A.unrelated to env
  Adding (root Irmin_layers_intf).B.unrelated to env
  Adding (root Irmin_layers_intf).B.conflicting_type to env
  Adding (root Irmin_layers_intf).B.Conflicting_module to env
  Handling include of : identifier((root Irmin_layers_intf).A, false)
  Removing (root Irmin_layers_intf).B.unrelated from env
  Removing (root Irmin_layers_intf).B.unrelated from env
  Adding (root Irmin_layers_intf).B.unrelated to env
  Adding (root Irmin_layers_intf).B.unrelated to env
  Adding (root Irmin_layers_intf).B.conflicting_type to env
  Adding (root Irmin_layers_intf).B.Conflicting_module to env
  Handling include of : identifier((root Irmin_layers_intf).A, false)
  Removing (root Irmin_layers_intf).B.unrelated from env
  Removing (root Irmin_layers_intf).B.unrelated from env
  Adding (root Irmin_layers_intf).B.unrelated to env
  Adding (root Irmin_layers_intf).C.unrelated to env
  Adding (root Irmin_layers_intf).C.conflicting_type to env
  Adding (root Irmin_layers_intf).C.Conflicting_module to env
  Handling include of : identifier((root Irmin_layers_intf).B, false)
  Removing (root Irmin_layers_intf).C.unrelated from env
  Removing (root Irmin_layers_intf).C.conflicting_type from env
  Removing (root Irmin_layers_intf).C.Conflicting_module from env
  Removing (root Irmin_layers_intf).C.unrelated from env
  Removing (root Irmin_layers_intf).C.conflicting_type from env
  Removing (root Irmin_layers_intf).C.Conflicting_module from env
  Adding (root Irmin_layers_intf).C.unrelated to env
  Adding (root Irmin_layers_intf).C.conflicting_type to env
  Adding (root Irmin_layers_intf).C.Conflicting_module to env
  Adding (root Irmin_layers_intf).C.unrelated to env
  Adding (root Irmin_layers_intf).C.conflicting_type to env
  Adding (root Irmin_layers_intf).C.Conflicting_module to env
  Handling include of : identifier((root Irmin_layers_intf).B, false)
  Removing (root Irmin_layers_intf).C.unrelated from env
  Removing (root Irmin_layers_intf).C.conflicting_type from env
  Removing (root Irmin_layers_intf).C.Conflicting_module from env
  Removing (root Irmin_layers_intf).C.unrelated from env
  Removing (root Irmin_layers_intf).C.conflicting_type from env
  Removing (root Irmin_layers_intf).C.Conflicting_module from env
  Adding (root Irmin_layers_intf).C.unrelated to env
  Adding (root Irmin_layers_intf).C.conflicting_type to env
  Adding (root Irmin_layers_intf).C.Conflicting_module to env
  Adding (root Irmin_layers_intf).Maker.hash to env
  Adding (root Irmin_layers_intf).Maker.Make to env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Adding (root Irmin_layers_intf).Maker.Make.result.conflicting_type to env
  Adding (root Irmin_layers_intf).Maker.Make.result.Conflicting_module to env
  Handling include of : r((root Irmin_layers_intf).B) with [root.unrelated = identifier(int, false)]
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Removing (root Irmin_layers_intf).Maker.Make.result.conflicting_type from env
  Removing (root Irmin_layers_intf).Maker.Make.result.Conflicting_module from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Adding (root Irmin_layers_intf).Maker.Make.result.conflicting_type to env
  Adding (root Irmin_layers_intf).Maker.Make.result.Conflicting_module to env
  Handling include of : r((root Irmin_layers_intf).A) with [root.unrelated = identifier(int, false)]
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Overriding duplicate env entry: conflicting_type
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Env.add_to_elts in file "src/xref2/env.ml", line 232, characters 8-24
        Called from Odoc_xref2__Env.add_type in file "src/xref2/env.ml", line 341, characters 12-72
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 226, characters 23-55
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 350, characters 10-45
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 246, characters 21-35
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 350, characters 10-45
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 246, characters 21-35
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.simple_expansion in file "src/xref2/compile.ml", line 366, characters 30-51
        Called from Odoc_xref2__Compile.module_type_expr.get_expansion in file "src/xref2/compile.ml", line 612, characters 17-44
        Called from Odoc_xref2__Compile.module_type_expr in file "src/xref2/compile.ml", line 626, characters 24-51
        Called from Odoc_xref2__Compile.module_type_expr in file "src/xref2/compile.ml", line 636, characters 17-55
        Called from Odoc_xref2__Compile.module_decl in file "src/xref2/compile.ml", line 293, characters 34-64
        Called from Odoc_xref2__Compile.module_ in file "src/xref2/compile.ml", line 287, characters 24-72
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 207, characters 21-34
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 202, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 18-48
        Called from Odoc_xref2__Compile.module_type_expr in file "src/xref2/compile.ml", line 619, characters 29-49
        Called from Odoc_xref2__Compile.module_type in file "src/xref2/compile.ml", line 310, characters 21-71
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 229, characters 21-39
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


