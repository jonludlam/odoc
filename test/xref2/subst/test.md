```ocaml
let resolve_module_name sg name =
  let rec check = function
    | Component.Signature.Module (id, _r, _m) :: _rest
      when Ident.Name.module_ id = name ->
        id
    | _ :: rest -> check rest
    | [] -> failwith "Unknown"
  in
  check sg.Component.Signature.items

let module_substitution ~idents ~targets m test_data =
  let _, sg, _ = Common.model_of_string test_data in

  let c = Component.Of_Lang.(signature (empty ()) sg) in

  let subst_idents_mod = resolve_module_name c idents in
  let subst_targets_mod = resolve_module_name c targets in

  let subst =
    let target = Cpath.Mk.Resolved.Module.local (subst_targets_mod :> Ident.path_module) in
    Subst.add_module
      (subst_idents_mod :> Ident.path_module)
      (Cpath.Mk.Module.resolved target) target Subst.identity
  in

  let m =
    match Find.module_in_sig c "S" with
    | Some (`FModule (name, m)) -> m
    | None -> failwith "Error finding module!"
  in

  let m' = Subst.module_ subst m in

  let open Format in
  fprintf std_formatter "BEFORE\n======\n%!";
  fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m;
  fprintf std_formatter "AFTER \n======\n%!";
  fprintf std_formatter "S%a\n\n%!" Component.Fmt.module_ m'
```
```mdx-error
Line 32, characters 34-35:
Error: This expression has type Component.Module.t Component.Delayed.t
       but an expression was expected of type Component.Module.t
```

Module substitution test

This test substitutes one module for another. We substitute
SubTargets in place of SubstituteMe, so the result expected is that
the equations for t, u and v point to SubTargets rather than SubstituteMe

```ocaml
# module_substitution ~idents:"SubstituteMe" ~targets:"SubTargets" "S" {|
  module SubstituteMe : sig
      type t
      type u
      type v
  end

  module SubTargets : sig
      type t
      type u
      type v
  end

  module S : sig
      type tt = SubstituteMe.t
      type uu = SubstituteMe.u
      type vv = SubstituteMe.v
  end
  |} ;;
Line 1, characters 1-20:
Error: Unbound value module_substitution
```

Now test by compiling signatures and printing the result:

```ocaml
(* Nicer output *)
#install_printer Component.Fmt.signature;;

let compile mli =
  let open Component in
  let id, sg, _ = Common.model_of_string mli in
  let env = Env.env_for_testing ~linking:false in
  Odoc_xref2.Compile.signature env (id :> Odoc_model.Paths.Identifier.Signature.t) sg
  |> Of_Lang.(signature (empty ()))
```

```ocaml
# compile {|
  module type Monad = sig
    type 'a t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val join : 'a t t -> 'a t
  end

  (** Simplest case *)
  module SomeMonad : sig
    type 'a t

    include Monad with type 'a t := 'a t
  end

  (** Substitute with a more complex type *)
  module ComplexTypeExpr : sig
    type ('a, 'b) t

    include Monad with type 'a t := (int, 'a) t * ('a, int) t
  end

  (** No abstraction *)
  module Erase : sig
    include Monad with type 'a t := 'a
  end
  |} ;;
- : Component.Signature.t =
module type Monad/92 ...
module SomeMonad/91 = : sig
  type t/93 ...
  include : r(Monad/92) with [r(root(Monad/92).t) = [a] r(t/93)] (sig =
    val map/94 ...
    val join/95 ...
     (removed=[]))
   (removed=[])end
module ComplexTypeExpr/90 = : sig
  type t/96 ...
  include : r(Monad/92) with [r(root(Monad/92).t) = ([r(int) * a] r(t/96) * [a * r(int)] r(t/96))] (sig =
    val map/97 ...
    val join/98 ...
     (removed=[]))
   (removed=[])end
module Erase/89 = : sig
  include : r(Monad/92) with [r(root(Monad/92).t) = a] (sig = val map/99 ...
                                                              val join/100 ...
                                                               (removed=[]))
   (removed=[])end
 (removed=[])
```

More tests with two type variables:

```ocaml
# compile {|
  module type Monad_2 = sig
    type ('a, 'err) t
    val map : ('a, 'err) t -> f:('a -> 'b) -> ('b, 'err) t
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
  end

  module SwappedVars : sig
    type ('x, 'y) t
    include Monad_2 with type ('a, 'b) t := ('b, 'a) t
  end
  |} ;;
- : Component.Signature.t =
module type Monad_2/151 ...
module SwappedVars/150 = : sig
  type t/152 ...
  include : r(Monad_2/151) with [r(root(Monad_2/151).t) = [b * a] r(t/152)] (sig =
    val map/153 ...
    val join/154 ...
    val both/155 ...
     (removed=[]))
   (removed=[])end
 (removed=[])
```

Edge cases:

```ocaml
# compile {|
  module type S = sig
    type 'a t
    val map : 'a t -> ('a -> 'b) -> 'b t
  end

  module M : sig
    type 'a t
    include S with type 'a t := ([ `A of 'a * 'b ] as 'b) t
  end
  |} ;;
- : Component.Signature.t =
module type S/182 ...
module M/181 = : sig
  type t/183 ...
  include : r(S/182) with [r(root(S/182).t) = [(alias (poly_var [ `A of (a * b) ]) b)] r(t/183)] (sig =
    val map/184 ...
     (removed=[]))
   (removed=[])end
 (removed=[])
```
