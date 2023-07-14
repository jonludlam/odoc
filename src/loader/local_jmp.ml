(* #if OCAML_VERSION >= (4, 14, 0) *)

(* open Odoc_model.Lang.Source_info *)

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

let ( let= ) m f = match m with Some x -> f x | None -> ()

type annotations =
  | LocalDef of string * string
  | LocalJmp of string
  | Def of Odoc_model.Names.DefName.t
  | DefJmp of Odoc_model.Names.DefName.t

module Analysis = struct
  let anchor_of_uid uid =
    match Uid.unpack_uid uid with
    | Some (_, Some id) -> Some (Uid.anchor_of_id id)
    | _ -> None

  (** Generate the anchors that will be pointed to by [lookup_def]. *)
  let _init poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let= s = anchor_of_uid uid in
        poses := (Def s, pos_of_loc t) :: !poses)
      uid_to_loc

  let _pat poses (type a) : a Typedtree.general_pattern -> unit = function
    | {
        pat_desc = Tpat_var (id, _stringloc) | Tpat_alias (_, id, _stringloc);
        pat_loc;
        _;
      }
        when not pat_loc.loc_ghost ->
          let uniq = Ident.unique_name id in
          let name = Ident.name id in
          poses := (LocalDef (uniq, name), pos_of_loc pat_loc) :: !poses
    | _ -> ()
  
  let _expr poses uid_to_loc expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (p, _, value_description); exp_loc; _ }
      -> (
        (* Only generate anchor if the uid is in the location table. We don't
           link to modules outside of the compilation unit. *)
        match Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid with
        | Some _ ->
          let= anchor = anchor_of_uid value_description.val_uid in
          poses := (DefJmp anchor, pos_of_loc exp_loc) :: !poses
        | None ->
          match p with
          | Pident id
          when not exp_loc.loc_ghost ->
            let anchor = Ident.unique_name id in
            poses := (LocalJmp anchor, pos_of_loc exp_loc) :: !poses
          | _ -> ())
    | _ -> ()
end


module Analysis2 = struct
  open Typedtree
  open Odoc_model.Paths

  let rec structure env parent str =
    let items = Ident_env.extract_structure_tree_items false str.str_items |> Ident_env.flatten_extracted in
    List.iter (fun item ->
      let id, loc =
        match item with
        | `Module (id, _, loc) -> id, loc
        | `ModuleType (id, _, loc) -> id, loc
        | `Type (id, _, loc) -> id, loc
        | `Value (id, _, loc) -> id, loc
        | `Class (id, _, _, _, _, loc) -> id, loc
        | `ClassType (id, _, _, _, loc) -> id, loc
      in
      let (s,e) = match loc with | Some l -> pos_of_loc l | None -> (0,0) in
      Format.eprintf "id: %a loc: (%d,%d)\n%!" Ident.print id s e
    ) items;
      
      let env = Ident_env.env_of_items parent items env in
    List.fold_left
      (fun items item ->
        List.rev_append (structure_item env parent item) items)
      [] str.str_items
    |> List.rev
  
  and signature env parent sg =
    let items = Ident_env.extract_signature_tree_items false sg |> Ident_env.flatten_extracted in
    List.iter (fun item ->
      let id, loc =
        match item with
        | `Module (id, _, loc) -> id, loc
        | `ModuleType (id, _, loc) -> id, loc
        | `Type (id, _, loc) -> id, loc
        | `Value (id, _, loc) -> id, loc
        | `Class (id, _, _, _, _, loc) -> id, loc
        | `ClassType (id, _, _, _, loc) -> id, loc
      in
      let (s,e) = match loc with | Some l -> pos_of_loc l | None -> (0,0) in
      Format.eprintf "id: %a loc: (%d,%d)\n%!" Ident.print id s e
    ) items;
    let _env' = Ident_env.env_of_items (parent :> Identifier.Signature.t) items env in
    []

  and structure_item env parent item =
    match item.str_desc with
    | Tstr_eval _ -> []
    | Tstr_value(_, vbs) ->
        value_bindings env parent vbs
    | Tstr_module mb -> begin
        match module_binding env parent mb with
        | Some mb ->
          [mb]
        | None -> []
        end
    | Tstr_recmodule mbs ->
        module_bindings env parent mbs
    | Tstr_modtype mtd ->
        module_type_decl env parent mtd
    | _ -> []

  and value_bindings env parent vbs =
    let items =
      List.fold_left
        (fun acc vb ->
           let vb = value_binding env parent vb in
            List.rev_append vb acc)
        [] vbs
    in
      List.rev items
  
  and value_binding _env _parent _vb =
    []
  
  and module_type_decl env _parent mtd =
    let id = Ident_env.find_module_type env mtd.mtd_id in
    match mtd.mtd_type with
    | None -> []
    | Some mty -> module_type env id mty

  and module_type env parent mty =
    match mty.mty_desc with
    | Tmty_signature sg -> signature env parent sg.sig_items
    | Tmty_with (mty, _) -> module_type env parent mty
    | Tmty_ident _ -> []
    | Tmty_functor (_, t) -> module_type env parent t 
    | Tmty_alias _ -> []
    | Tmty_typeof _ -> []

  and module_bindings env parent mbs =
    let items =
      List.fold_left
        (fun acc vb ->
           match module_binding env parent vb with
           | Some mb -> mb :: acc
           | None -> acc)
        [] mbs
    in
      List.rev items
  and module_binding env _parent mb =
    match mb.mb_id with
    | None -> None
    | Some id ->
      let id = Ident_env.find_module_identifier env id in
      let id = (id :> Identifier.Module.t) in
      let _inner =
        match unwrap_module_expr_desc mb.mb_expr.mod_desc with
        | Tmod_ident (_p, _) -> []
        | _ ->
            let id = (id :> Identifier.Signature.t) in
            module_expr env id mb.mb_expr
      in
      None

  and module_expr env parent mexpr =
    let open Odoc_model.Names in
    match mexpr.mod_desc with
    | Tmod_ident _ ->
      []
    | Tmod_structure str ->
        let sg = structure env parent str in
        sg
    | Tmod_functor(parameter, res) ->
        let _f_parameter, env =
          match parameter with
          | Unit -> [], env
          | Named (id_opt, _, _arg) ->
              let name, env =
                match id_opt with
                | Some id -> Ident.name id, Ident_env.add_parameter parent id (ModuleName.of_ident id) env
                | None -> "_", env
              in
              let _id = Odoc_model.Paths.Identifier.Mk.parameter (parent, Odoc_model.Names.ModuleName.make_std name) in
              [], env
          in
        let res = module_expr env (Odoc_model.Paths.Identifier.Mk.result parent) res in
        res
    | _ ->
      []

  and unwrap_module_expr_desc = function
    | Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
    | desc -> desc

end




let of_cmt (id : Odoc_model.Paths.Identifier.RootModule.t) (cmt : Cmt_format.cmt_infos) =
  let ttree = cmt.cmt_annots in
  match ttree with
  | Cmt_format.Implementation structure ->
      let uid_to_loc = cmt.cmt_uid_to_loc in
      let env = Ident_env.empty () in 
      let _ = Analysis2.structure env (id :> Odoc_model.Paths.Identifier.Signature.t) structure in
      Shape.Uid.Tbl.iter (fun shape loc ->
        let id = Ident_env.identifier_of_loc env loc in
        let name = match id with
          | Some id -> Odoc_model.Paths.Identifier.name id
          | None -> let (x,y) = pos_of_loc loc in Format.asprintf "unknown (%d,%d)" x y
        in
        Format.eprintf "%a: %s\n%!" Shape.Uid.print shape name) uid_to_loc;
        []
  | _ -> []

(* #else

let of_cmt _ = []

#endif *)
