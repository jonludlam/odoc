```ocaml env=e1
(* Prelude *)
#require "odoc.xref_test";;
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 6000;;
#print_depth 20;;
```

```ocaml env=e1
let test_data = {|
module type RESULT = sig type u type v end
module Make (S : sig type t end)
  : Compute_ranges_intf.S
      with type u := S.t
           type v = S.t
|}
let sg = Common.signature_of_mli_string test_data;;
```

```ocaml env=e1
# sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "t");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params =
      [(Odoc_model.Lang.TypeDecl.Var "a", None);
       (Odoc_model.Lang.TypeDecl.Var "b", None)];
     private_ = false;
     manifest =
      Some
       (Odoc_model.Lang.TypeExpr.Tuple
         [Odoc_model.Lang.TypeExpr.Var "a"; Odoc_model.Lang.TypeExpr.Var "b"]);
     constraints = []};
   representation = None});
 Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "u");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params =
      [(Odoc_model.Lang.TypeDecl.Var "c", None)];
     private_ = false;
     manifest =
      Some
       (Odoc_model.Lang.TypeExpr.Constr
         (`Resolved (`Identifier (`Type (`Root (Common.root, "Root"), "t"))),
         [Odoc_model.Lang.TypeExpr.Constr
           (`Resolved (`Identifier (`CoreType "float")), []);
          Odoc_model.Lang.TypeExpr.Var "c"]));
     constraints = []};
   representation = None})]
```
