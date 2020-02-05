```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 60;;
#print_depth 3;;
```

```ocaml env=e1
let f = Odoc_odoc.Fs.File.of_string "/Users/jon/devel/base.v0.13.0/_build/default/src/.base.objs/byte/base.odocl";;
let base = Odoc_odoc.Compilation_unit.load f;;
let contents = base.Odoc_model.Lang.Compilation_unit.content;;
let Odoc_model.Lang.Compilation_unit.Module m = contents;;
let array_lens = Common.LangUtils.Lens.Signature.module_ "Array";;
let array = Common.LangUtils.Lens.get array_lens m;;
let Some expn = array.Odoc_model.Lang.Module.expansion;;
let Odoc_model.Lang.Module.Signature sg = expn;;
```

```ocaml env=e1
let f = Odoc_odoc.Fs.File.of_string "/Users/jon/devel/base.v0.13.0/_build/default/src/.base.objs/byte/base__array.odoc";;
let array = Odoc_odoc.Compilation_unit.load f;;
let contents = array.Odoc_model.Lang.Compilation_unit.content;;
let Odoc_model.Lang.Compilation_unit.Module m = contents;;
#print_depth 15;;

```

```ocaml env=e1
# let _::_::inc::_ = sg;;
Line 1, characters 5-17:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_::_::[]|_::[]|[])
val inc : Odoc_model.Lang.Signature.item =
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent =
     `Module (`Root (Common.root, "Base"), "Array");
    doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`ModuleType
                (`Module
                   (`Hidden (`Identifier (`Root (Common.root, "Base__"))),
                    "Binary_searchable"),
                 "S1"))),
        [Odoc_model.Lang.ModuleType.TypeSubst
          (`Resolved (`Type (`Root, "t")),
          {Odoc_model.Lang.TypeDecl.Equation.params =
            [(Odoc_model.Lang.TypeDecl.Var ...); ...]; private_ = ...;
            manifest = ...; constraints = ...});
          ...]));
     expansion = ...}
```

```ocaml env=e1
# let _::_::inc'::_ = m;;
Line 1, characters 5-18:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_::_::[]|_::[]|[])
val inc' : Odoc_model.Lang.Signature.item =
  Odoc_model.Lang.Signature.Include
   {Odoc_model.Lang.Include.parent = `Root (Common.root, "Base__Array");
    doc = [];
    decl =
     Odoc_model.Lang.Module.ModuleType
      (Odoc_model.Lang.ModuleType.With
        (Odoc_model.Lang.ModuleType.Path
          (`Resolved
             (`ModuleType
                (`Canonical
                   (`Module
                      (`Hidden (`Identifier (`Root (...))),
                       "Binary_searchable"),
                    `Dot
                      (`Root "Base",
                       "Binary_searchab"... (* string length 17; truncated *))),
                 "S1"))),
        [Odoc_model.Lang.ModuleType.TypeSubst
          (`Resolved (`Type (`Root, "t")),
          {Odoc_model.Lang.TypeDecl.Equation.params = ...; private_ = ...;
           manifest = ...; constraints = ...});
         ...]));
    expansion = ...}
```
