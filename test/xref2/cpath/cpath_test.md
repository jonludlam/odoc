### Hidden paths

Tests for `weak_canonical_test`. When calling `is_resolved_module_hidden`, we normally would have `weak_canonical_test` false, and we'll check to see whether the canonical part of a path is hidden or not. When `weak_canonical_test` is true, we'll _assume_ that the canonical part will resolve to be a non-hidden path.

In the following test, we create a path with an unresolved canonical 
```ocaml env=e1
# open Odoc_xref2.Cpath.Mk;;
# let m = Resolved.Module.(hidden (local (`LModule (Odoc_model.Names.ModuleName.internal_of_string "M", 1))));;
val m : Odoc_xref2.Cpath.Resolved.module_ =
  {Odoc_xref2.Hc.v =
    `Hidden
      {Odoc_xref2.Hc.v = `Local (`LModule ({M}1, 1));
       key = (4, "predefined")};
   key = (5, "predefined")}
# let cm = `Root "Foo";;
val cm : [> `Root of string ] = `Root "Foo"
# let p = Resolved.Module.canonical (m, cm);;
val p : Odoc_xref2.Cpath.Resolved.module_ =
  {Odoc_xref2.Hc.v =
    `Canonical
      ({Odoc_xref2.Hc.v =
         `Hidden
           {Odoc_xref2.Hc.v = `Local (`LModule ({M}1, 1));
            key = (4, "predefined")};
        key = (5, "predefined")},
       `Root "Foo");
   key = (6, "predefined")}
# let r1 = Cpath.is_resolved_module_hidden ~weak_canonical_test:false p;;
val r1 : bool = true
# let r2 = Cpath.is_resolved_module_hidden ~weak_canonical_test:true p;;
val r2 : bool = false
```

