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
    let target = `Local (subst_targets_mod :> Ident.path_module) in
    Subst.add_module
      (subst_idents_mod :> Ident.path_module)
      (`Resolved target) target Subst.identity
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
BEFORE
======
S: sig
type tt/5 = local(SubstituteMe/2,false).t
type uu/4 = local(SubstituteMe/2,false).u
type vv/3 = local(SubstituteMe/2,false).v
 (removed=[])end (canonical=None)

AFTER
======
S: sig
type tt/6 = r(SubTargets/1).t
type uu/7 = r(SubTargets/1).u
type vv/8 = r(SubTargets/1).v
 (removed=[])end (canonical=None)

- : unit = ()
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
module type Monad/30 = sig
  type t/31
  val map/32 : ([a] r(t/31)) -> ((a) -> b) -> [b] r(t/31)
  val join/33 : ([[a] r(t/31)] r(t/31)) -> [a] r(t/31)
   (removed=[])end
module SomeMonad/29 : sig
  type t/35
  include : (w) r(Monad/30) with [r(root(Monad/30).t) = [a] r(t/35)] (sig = Some(
    val map/36 : ([a] r(t/35)) -> ((a) -> b) -> [b] r(t/35)
    val join/37 : ([[a] r(t/35)] r(t/35)) -> [a] r(t/35)
     (removed=[type (a) t/35 = ([a] local(t/35,false))])))
   (removed=[])end (canonical=None)
module ComplexTypeExpr/28 : sig
  type t/39
  include : (w) r(Monad/30) with [r(root(Monad/30).t) = ([r(int) * a] r(t/39) * [a * r(int)] r(t/39))] (sig = Some(
    val map/40 : (([r(int) * a] r(t/39) * [a * r(int)] r(t/39))) -> ((a) -> b) -> ([r(int) * b] r(t/39) * [b * r(int)] r(t/39))
    val join/41 : (([r(int) * ([r(int) * a] r(t/39) * [a * r(int)] r(t/39))] r(t/39) * [([r(int) * a] r(t/39) * [a * r(int)] r(t/39)) * r(int)] r(t/39))) -> ([r(int) * a] r(t/39) * [a * r(int)] r(t/39))
     (removed=[type (a) t/39 = (([identifier(int, false) * a] local(t/39,false) * [a * identifier(int, false)] local(t/39,false)))])))
   (removed=[])end (canonical=None)
module Erase/27 : sig
  include : (w) r(Monad/30) with [r(root(Monad/30).t) = a] (sig = Some(
    val map/43 : (a) -> ((a) -> b) -> b
    val join/44 : (a) -> a
     (removed=[type (a) t/42 = (a)])))
   (removed=[])end (canonical=None)
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
module type Monad_2/57 = sig
  type t/58
  val map/59 : ([a * err] r(t/58)) -> f:((a) -> b) -> [b * err] r(t/58)
  val join/60 : ([[a * e] r(t/58) * e] r(t/58)) -> [a * e] r(t/58)
  val both/61 : ([a * e] r(t/58)) -> ([b * e] r(t/58)) -> [(a * b) * e] r(t/58)
   (removed=[])end
module SwappedVars/56 : sig
  type t/63
  include : (w) r(Monad_2/57) with [r(root(Monad_2/57).t) = [b * a] r(t/63)] (sig = Some(
    val map/64 : ([err * a] r(t/63)) -> f:((a) -> b) -> [err * b] r(t/63)
    val join/65 : ([e * [e * a] r(t/63)] r(t/63)) -> [e * a] r(t/63)
    val both/66 : ([e * a] r(t/63)) -> ([e * b] r(t/63)) -> [e * (a * b)] r(t/63)
     (removed=[type (a, b) t/63 = ([b * a] local(t/63,false))])))
   (removed=[])end (canonical=None)
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
module type S/73 = sig
  type t/74
  val map/75 : ([a] r(t/74)) -> ((a) -> b) -> [b] r(t/74)
   (removed=[])end
module M/72 : sig
  type t/77
  include : (w) r(S/73) with [r(root(S/73).t) = [(alias (poly_var [ `A of (a * b) ]) b)] r(t/77)] (sig = Some(
    val map/78 : ([(alias (poly_var [ `A of (a * b) ]) b)] r(t/77)) -> ((a) -> b) -> [(alias (poly_var [ `A of (b * b) ]) b)] r(t/77)
     (removed=[type (a) t/77 = ([(alias (poly_var [ `A of (a * b) ]) b)] local(t/77,false))])))
   (removed=[])end (canonical=None)
 (removed=[])
```
