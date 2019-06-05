open Odoc__xref2
open Odoc_xref_test

let filter_map f m =
    let rec inner = function
    | x::xs -> begin match f x with | Some x' -> x'::inner xs | None -> inner xs end
    | [] -> []
    in inner m 

let resolve_module_name sg name =
    let rec inner = function
        | Component.Signature.Module (id, _) :: rest ->
            if Ident.name id = name then id else inner rest
        | _::rest -> inner rest
        | _ -> raise Not_found
    in
    inner sg

let test_data = {|

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

(* The test substitutes SubTargets in place of SubstituteMe, so the result expected is that
   the equations for t, u and v point to SubTargets rather than SubstituteMe *)
module S : sig
    open SubstituteMe
    type tt = t
    type uu = u
    type vv = v
end

|}


let test_subst () =
    let _, _, sg = Common.model_of_string test_data in

    let c = Component.Of_Lang.of_signature [] sg in

    let subst_idents_mod = resolve_module_name c "SubstituteMe" in
    let subst_targets_mod = resolve_module_name c "SubTargets" in

    let subst = Subst.add subst_idents_mod (`Local subst_targets_mod) Subst.identity in

    let subst_object = Component.Find.module_in_sig c "S" in

    let m' = Subst.module_ subst subst_object in

    Format.fprintf Format.std_formatter "%a%!" Component.Fmt.module_ m'

let _ =
    test_subst ()