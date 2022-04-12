```ocaml
let simple_strengthening input =
    let p = Common.root_identifier in
    let _, sg, _ = Common.model_of_string input in
    let c = Component.Of_Lang.(signature (empty ()) sg) in
    let cp = Component.Of_Lang.(resolved_module_path (empty ()) p) in
    let c' = Strengthen.signature (`Resolved cp) c in
    let open Format in
    fprintf std_formatter "BEFORE\n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c;
    fprintf std_formatter "AFTER \n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c'
```
```mdx-error
Line 5, characters 67-68:
Error: This expression has type
         [> `Identifier of
              [> `Root of
                   [> `Page of 'a option * Odoc_model.Names.PageName.t ]
                   option * Odoc_model.Names.ModuleName.t ] ]
       but an expression was expected of type
         Odoc_model.Paths.Path.Resolved.Module.t =
           Odoc_model__Paths_types.Resolved_path.module_unhashed
           Odoc_model.Hc.hashed
```

Simple strengthening

This tests that strengthening works. When we strengthen a signature we recursively add
type equations for all abstract types.

```ocaml
# simple_strengthening {|
  type t
  type u = t
  |} ;;
Line 1, characters 1-21:
Error: Unbound value simple_strengthening
```
