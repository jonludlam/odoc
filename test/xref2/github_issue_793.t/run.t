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
  Overriding duplicate env entry: Conflicting_module
  Overriding duplicate env entry: Conflicting_module_type
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Removing (root Irmin_layers_intf).Maker.Make.result.conflicting_type from env
  Removing (root Irmin_layers_intf).Maker.Make.result.Conflicting_module from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Adding (root Irmin_layers_intf).Maker.Make.result.conflicting_type to env
  Adding (root Irmin_layers_intf).Maker.Make.result.Conflicting_module to env
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
  Overriding duplicate env entry: Conflicting_module
  Overriding duplicate env entry: Conflicting_module_type
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Removing (root Irmin_layers_intf).Maker.Make.result.unrelated from env
  Removing (root Irmin_layers_intf).Maker.Make.result.conflicting_type from env
  Removing (root Irmin_layers_intf).Maker.Make.result.Conflicting_module from env
  Adding (root Irmin_layers_intf).Maker.Make.result.unrelated to env
  Adding (root Irmin_layers_intf).Maker.Make.result.conflicting_type to env
  Adding (root Irmin_layers_intf).Maker.Make.result.Conflicting_module to env


