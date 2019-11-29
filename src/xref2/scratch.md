```ocaml require=odoc.xref_test,env=e1
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 65536;;
```

```ocaml env=e1
let test_data = {|
module X(Y : sig type t end) : sig
  module type A = sig
    type u
  end
end

module B : sig
  type t
end

module type Z = X(B).A with type u := int
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

```ocaml env=e1
# sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.Module.id = `Module (`Root (Common.root, "Root"), "X");
   doc = [];
   type_ =
    Odoc_model.Lang.Module.ModuleType
     (Odoc_model.Lang.ModuleType.Functor
       (Some
         {Odoc_model.Lang.FunctorArgument.id =
           `Parameter (`Module (`Root (Common.root, "Root"), "X"), "Y");
          expr =
           Odoc_model.Lang.ModuleType.Signature
            [Odoc_model.Lang.Signature.Type
              (Odoc_model.Lang.Signature.Ordinary,
              {Odoc_model.Lang.TypeDecl.id =
                `Type
                  (`Parameter
                     (`Module (`Root (Common.root, "Root"), "X"), "Y"),
                   "t");
               doc = [];
               equation =
                {Odoc_model.Lang.TypeDecl.Equation.params = [];
                 private_ = false; manifest = None; constraints = []};
               representation = None})];
          expansion = Some Odoc_model.Lang.Module.AlreadyASig},
       Odoc_model.Lang.ModuleType.Signature
        [Odoc_model.Lang.Signature.ModuleType
          {Odoc_model.Lang.ModuleType.id =
            `ModuleType
              (`Result (`Module (`Root (Common.root, "Root"), "X")), "A");
           doc = [];
           expr =
            Some
             (Odoc_model.Lang.ModuleType.Signature
               [Odoc_model.Lang.Signature.Type
                 (Odoc_model.Lang.Signature.Ordinary,
                 {Odoc_model.Lang.TypeDecl.id =
                   `Type
                     (`ModuleType
                        (`Result (`Module (`Root (Common.root, "Root"), "X")),
                         "A"),
                      "u");
                  doc = [];
                  equation =
                   {Odoc_model.Lang.TypeDecl.Equation.params = [];
                    private_ = false; manifest = None; constraints = []};
                  representation = None})]);
           expansion = Some Odoc_model.Lang.Module.AlreadyASig}]));
   canonical = None; hidden = false; display_type = None; expansion = None});
 Odoc_model.Lang.Signature.Module (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.Module.id = `Module (`Root (Common.root, "Root"), "B");
   doc = [];
   type_ =
    Odoc_model.Lang.Module.ModuleType
     (Odoc_model.Lang.ModuleType.Signature
       [Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
         {Odoc_model.Lang.TypeDecl.id =
           `Type (`Module (`Root (Common.root, "Root"), "B"), "t");
          doc = [];
          equation =
           {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
            manifest = None; constraints = []};
          representation = None})]);
   canonical = None; hidden = false; display_type = None;
   expansion = Some Odoc_model.Lang.Module.AlreadyASig});
 Odoc_model.Lang.Signature.ModuleType
  {Odoc_model.Lang.ModuleType.id =
    `ModuleType (`Root (Common.root, "Root"), "Z");
   doc = [];
   expr =
    Some
     (Odoc_model.Lang.ModuleType.With
       (Odoc_model.Lang.ModuleType.Path
         (`Dot
            (`Apply
               (`Resolved
                  (`Identifier (`Module (`Root (Common.root, "Root"), "X"))),
                `Resolved
                  (`Identifier (`Module (`Root (Common.root, "Root"), "B")))),
             "A")),
       [Odoc_model.Lang.ModuleType.TypeSubst (`Dot (`Resolved `Root, "u"),
         {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
          manifest =
           Some
            (Odoc_model.Lang.TypeExpr.Constr
              (`Resolved (`Identifier (`CoreType "int")), []));
          constraints = []})]));
   expansion = None}]
```
