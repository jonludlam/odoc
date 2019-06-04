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

let test_subst () =
    let filename = "subst.mli.input" in
    let mli = Common.string_of_file filename in
    let _, _, sg = Common.model_of_string mli in
    let c = Component.Of_Lang.of_signature [] sg in

    let subst_idents_mod = resolve_module_name c "SubstituteMe" in
    let subst_targets_mod = resolve_module_name c "SubTargets" in

    let subst = Subst.add subst_idents_mod (`Local subst_targets_mod) Subst.identity in

    let subst_object = Test.find_module_in_sig c "S" in

    let m' = Subst.module_ subst subst_object in

    Format.fprintf Format.std_formatter "%a%!" Component.Fmt.module_ m'

let _ =
    test_subst ()