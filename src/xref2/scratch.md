```ocaml require=odoc.xref_test,env=e1
open Odoc_xref2;;
open Odoc_xref_test;;
#install_printer Common.root_pp;;
#print_length 65536;;
```

```ocaml env=e1
let test_data = {|
type a = A | B of { foo: int }
type c =
    { d : int
    ; f : a }
|};;
let sg = Common.signature_of_mli_string test_data;;
let env = Env.open_signature sg Env.empty;;
```

```ocaml env=e1
# sg;;
- : Odoc_model.Lang.Signature.t =
[Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "a");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
     manifest = None; constraints = []};
   representation =
    Some
     (Odoc_model.Lang.TypeDecl.Representation.Variant
       [{Odoc_model.Lang.TypeDecl.Constructor.id =
          `Constructor (`Type (`Root (Common.root, "Root"), "a"), "A");
         doc = []; args = Odoc_model.Lang.TypeDecl.Constructor.Tuple [];
         res = None};
        {Odoc_model.Lang.TypeDecl.Constructor.id =
          `Constructor (`Type (`Root (Common.root, "Root"), "a"), "B");
         doc = [];
         args =
          Odoc_model.Lang.TypeDecl.Constructor.Record
           [{Odoc_model.Lang.TypeDecl.Field.id =
              `Field (`Type (`Root (Common.root, "Root"), "a"), "foo");
             doc = []; mutable_ = false;
             type_ =
              Odoc_model.Lang.TypeExpr.Constr
               (`Resolved (`Identifier (`CoreType "int")), [])}];
         res = None}])});
 Odoc_model.Lang.Signature.Type (Odoc_model.Lang.Signature.Ordinary,
  {Odoc_model.Lang.TypeDecl.id = `Type (`Root (Common.root, "Root"), "c");
   doc = [];
   equation =
    {Odoc_model.Lang.TypeDecl.Equation.params = []; private_ = false;
     manifest = None; constraints = []};
   representation =
    Some
     (Odoc_model.Lang.TypeDecl.Representation.Record
       [{Odoc_model.Lang.TypeDecl.Field.id =
          `Field (`Type (`Root (Common.root, "Root"), "c"), "d");
         doc = []; mutable_ = false;
         type_ =
          Odoc_model.Lang.TypeExpr.Constr
           (`Resolved (`Identifier (`CoreType "int")), [])};
        {Odoc_model.Lang.TypeDecl.Field.id =
          `Field (`Type (`Root (Common.root, "Root"), "c"), "f");
         doc = []; mutable_ = false;
         type_ =
          Odoc_model.Lang.TypeExpr.Constr
           (`Resolved
              (`Identifier (`Type (`Root (Common.root, "Root"), "a"))),
           [])}])})]
```
