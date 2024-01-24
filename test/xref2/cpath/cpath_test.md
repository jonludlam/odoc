### Hidden paths

Tests for `weak_canonical_test`. When calling `is_resolved_module_hidden`, we normally would have `weak_canonical_test` false, and we'll check to see whether the canonical part of a path is hidden or not. When `weak_canonical_test` is true, we'll _assume_ that the canonical part will resolve to be a non-hidden path.

In the following test, we create a path with an unresolved canonical 
```ocaml env=e1
# let m = `Hidden (`Local (`LModule (Odoc_model.Names.ModuleName.internal_of_string "M", 1)));;
Line 1, characters 36-82:
Error: Unbound value Odoc_model.Names.ModuleName.internal_of_string
# let cm = `Root "Foo";;
val cm : [> `Root of string ] = `Root "Foo"
# let p = `Canonical(m, cm);;
Line 1, characters 20-21:
Error: Unbound value m
# let r1 = Cpath.is_resolved_module_hidden ~weak_canonical_test:false p;;
Line 1, characters 69-70:
Error: Unbound value p
# let r2 = Cpath.is_resolved_module_hidden ~weak_canonical_test:true p;;
Line 1, characters 68-69:
Error: Unbound value p
```

