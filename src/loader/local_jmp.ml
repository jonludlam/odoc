#if OCAML_VERSION >= (4, 14, 0)

let unpack_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> Some (s, None)
  | Item { comp_unit; id } -> Some (comp_unit, Some (string_of_int id))
  | Predef _ -> None
  | Internal -> None
  
open Odoc_model.Lang.Source_info

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)
let pp_loc ppf (x,y) =
  Format.fprintf ppf "(%d,%d)" x y

let ( let= ) m f = match m with Some x -> f x | None -> ()

module Local_analysis = struct
  let expr poses expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (Pident id, _, _); exp_loc; _ }
      when not exp_loc.loc_ghost ->
        let anchor = Ident.unique_name id in
        Format.eprintf "Adding a local occurrence for %s (pos %a)\n" anchor pp_loc (pos_of_loc exp_loc);
        poses := (Occurence { anchor }, pos_of_loc exp_loc) :: !poses
    | _ -> ()
  let pat poses (type a) : a Typedtree.general_pattern -> unit = function
    | {
        pat_desc = Tpat_var (id, _stringloc) | Tpat_alias (_, id, _stringloc);
        pat_loc;
        _;
      }
      when not pat_loc.loc_ghost ->
        let uniq = Ident.unique_name id in
        Format.eprintf "Adding a 'Def' for '%s' at loc %a\n%!" uniq pp_loc (pos_of_loc pat_loc);
        poses := (Def uniq, pos_of_loc pat_loc) :: !poses
    | _ -> ()
end

module Global_analysis = struct
  let anchor_of_uid uid =
    match unpack_uid uid with
    | Some (_, Some id) -> Some (Uid.anchor_of_id id)
    | _ -> None

  (** Generate the anchors that will be pointed to by [lookup_def]. *)
  let init poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let= s = anchor_of_uid uid in
        Format.eprintf "Adding a 'Def' for '%s' at loc %a\n%!" s pp_loc (pos_of_loc t);
        poses := (Def s, pos_of_loc t) :: !poses)
      uid_to_loc

  let init2 shape =
    let anchor_of x =
      List.map (fun (name,ty) ->
        match ty with
        | Shape.Sig_component_kind.Value -> "val-" ^ name
        | Type -> "type-" ^ name
        | Module -> "module-" ^ name
        | Module_type -> "module-type-" ^ name
        | Extension_constructor -> "ext-" ^ name
        | Class -> "class-" ^ name
        | Class_type -> "class-type-" ^ name) x |> List.rev |> String.concat "."
    in
    let rec inner shape uid_names cur =
      let add uid =
        match anchor_of_uid uid with
        | None -> uid_names
        | Some _ -> (uid, anchor_of cur) :: uid_names
      in
      match shape.Shape.uid with
      | None -> uid_names
      | Some uid ->
        match shape.Shape.desc with
        | Shape.Struct m -> (
          Shape.Item.Map.fold (fun item s uid_names ->
            let (name,ty) : (string * Shape.Sig_component_kind.t) = Obj.magic item in
            inner s uid_names ((name,ty)::cur)) m (add uid))
        | Abs (_, s) ->
          inner s (add uid) cur
        | App (_, _) ->
          add uid
        | Leaf 
        | Var _ 
        | Proj (_, _)
        | Comp_unit _ ->
            add uid
    in
  inner shape [] []

  let expr poses uid_to_loc expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (p, _, value_description); exp_loc; _ }
      ->
        (* Only generate anchor if the uid is in the location table. We don't
           link to modules outside of the compilation unit. *)
        let= _ = Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid in
        let= anchor = anchor_of_uid value_description.val_uid in
        Format.eprintf "Adding a global occurrence for %s (name %s) (pos %a)\n" anchor (Path.last p) pp_loc (pos_of_loc exp_loc);
        poses := (Occurence { anchor }, pos_of_loc exp_loc) :: !poses
    | _ -> ()
end

module Analysis = struct
(* 
  let rec structure uid_to_loc parent (s : Typedtree.structure) =
    List.fold_left (fun acc item ->
      List.rev_append (structure_item uid_to_loc parent item) acc) s.str_items

  and structure_item uid_to_loc parent (s : Typedtree.structure_item) =
      match s.str_desc with
      | Tstr_eval _ -> []
      | Tstr_value (_, vbs) -> value_bindings uid_to_loc parent vbs
      | Tstr_primitive vd -> [value_description uid_to_loc parent vd]
      | Tstr_type (_, decls) -> type_declarations uid_to_loc parent decls
      | Tstr_typext tyext -> [type_extension uid_to_loc parent tyext]
      | Tstr_exception exn -> [exception_ uid_to_loc parent exn]
      | Tstr_module mb -> [module_binding uid_to_loc parent mb]
      | Tstr_recmodule mbs -> module_bindings uid_to_loc parent mbs
      | Tstr_modtype mtd -> [module_type uid_to_loc parent mtd]
      | Tstr_open _ -> []
      | Tstr_include _ -> []
      | Tstr_class _ -> []
      | Tstr_class_type _ -> []
      | Tstr_attribute _ -> []

  and value_bindings uid_to_loc parent vbs =
      let items = List.fold_left (fun acc vb ->
        match value_binding uid_to_loc parent vb with
        | Some x -> x::acc
        | None -> acc) [] vbs in
      List.rev items
  and value_binding uid_to_loc parent vb =
      pattern uid_to_loc parent vb.vb_pat

  and pattern uid_to_loc parent pat =
    match pat.pat_desc with
    | Tpat_any -> None
    | Tpat_var (id) ->
      match Shape.Uid.Tbl.find_opt uid_to_loc pat.pat with


       *)

      

end

let of_cmt (cmt : Cmt_format.cmt_infos) =
  let ttree = cmt.cmt_annots in
  (match cmt.cmt_impl_shape with | Some x -> Format.eprintf "Shape: %a\n%!" Shape.print x | None -> ());
  (match cmt.cmt_impl_shape with
   | Some {desc=Struct _; _} -> Format.eprintf "Struct\n%!"
   | Some _ -> Format.eprintf "Not struct\n%!"
   | None -> ());
  match ttree with
  | Cmt_format.Implementation structure ->
      let uid_to_loc = cmt.cmt_uid_to_loc in
      let poses = ref [] in
      Global_analysis.init poses uid_to_loc;
      let uids = (match cmt.cmt_impl_shape with | Some x -> Global_analysis.init2 x | None -> []) in
      let uid_to_anchor = Shape.Uid.Tbl.of_list uids in
      let nuids = Shape.Uid.Tbl.length uid_to_loc in
      Format.eprintf "uids (%d calculated vs %d expected): [%s]" (List.length uids) nuids (String.concat "," (List.map snd uids));
      let expr iterator expr =
        Local_analysis.expr poses expr;
        Global_analysis.expr poses uid_to_loc expr;
        Tast_iterator.default_iterator.expr iterator expr
      in
      let pat iterator pat =
        Local_analysis.pat poses pat;
        Tast_iterator.default_iterator.pat iterator pat
      in
      let iterator = { Tast_iterator.default_iterator with expr; pat } in
      iterator.structure iterator structure;
      !poses
  | _ -> []

#else

let of_cmt _ = []

#endif
