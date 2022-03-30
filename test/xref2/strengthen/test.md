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
Line 6, characters 37-51:
Error: This expression has type [> `Resolved of Cpath.Resolved.module_ ]
       but an expression was expected of type
         Cpath.module_ = Cpath.module_unhashed Hc.hashed
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
