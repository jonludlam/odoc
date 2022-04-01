test2.md


```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
open Odoc_model.Names;;
#install_printer Common.root_pp;;
#install_printer Odoc_model.Names.ValueName.fmt;;
#install_printer Odoc_model.Names.ModuleName.fmt;;
#install_printer Odoc_model.Names.ModuleTypeName.fmt;;
#install_printer Odoc_model.Names.TypeName.fmt;;
#install_printer Odoc_model.Names.ParameterName.fmt;;
#install_printer Odoc_model.Names.ExceptionName.fmt;;
#install_printer Odoc_model.Names.FieldName.fmt;;
#install_printer Odoc_model.Names.PageName.fmt;;
#print_length 65536;;
Odoc_xref2.Component.Delayed.eager := true;;
Odoc_xref2.Tools.disable_all_caches ();;
let id = Common.id;;
```

```ocaml env=e1
let test_data = {|
  module A : sig
      type ext
  end
  module M : sig
    type t
    val f : t
    val ext : A.ext
  end
|};;
let sg = Common.signature_of_mli_string test_data;;
let sg = Common.compile_signature sg;;
let env = Env.open_signature sg Env.empty;;
```

```ocaml env=e1
# Env.lookup_by_name Env.s_module "M" env;;
- : Component.Element.module_ Env.maybe_ambiguous =
Result.Ok
 (`Module
    (`Module (`Root (Some (`Page (None, None)), Root), M),
     {Odoc_xref2.Component.Delayed.v =
       Some
        {Odoc_xref2.Component.Module.doc = [];
         type_ =
          Odoc_xref2.Component.Module.ModuleType
           (Odoc_xref2.Component.ModuleType.Signature
             {Odoc_xref2.Component.Signature.items =
               [Odoc_xref2.Component.Signature.Type (`LType (t, 19),
                 Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_xref2.Component.Delayed.v =
                   Some
                    {Odoc_xref2.Component.TypeDecl.doc = [];
                     canonical = None;
                     equation =
                      {Odoc_xref2.Component.TypeDecl.Equation.params = [];
                       private_ = false; manifest = None; constraints = []};
                     representation = None};
                  get = None});
                Odoc_xref2.Component.Signature.Value (`LValue (f, 20),
                 {Odoc_xref2.Component.Delayed.v =
                   Some
                    {Odoc_xref2.Component.Value.doc = [];
                     type_ =
                      Odoc_xref2.Component.TypeExpr.Constr
                       (`Resolved (162446286, `Local (`LType (t, 19))),
                       []);
                     value = Odoc_model.Lang.Value.Abstract};
                  get = None});
                Odoc_xref2.Component.Signature.Value (`LValue (ext, 21),
                 {Odoc_xref2.Component.Delayed.v =
                   Some
                    {Odoc_xref2.Component.Value.doc = [];
                     type_ =
                      Odoc_xref2.Component.TypeExpr.Constr
                       (`Resolved
                          (978184609,
                           `Type
                             (`Module
                                (488081873,
                                 `GPath
                                   (62747767,
                                    `Identifier
                                      (`Module
                                         (`Root
                                            (Some (`Page (None, None)), Root),
                                          A)))),
                              ext)),
                       []);
                     value = Odoc_model.Lang.Value.Abstract};
                  get = None})];
              compiled = true; removed = []; doc = []});
         canonical = None; hidden = false};
      get = None}))
```
