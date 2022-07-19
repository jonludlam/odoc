(* Note for future improvement (suggested by @lpw25):

   You can actually do something even more interesting with that case for strengthening.

   If you have:
   [module type S = sig type t end]

   and you want to strengthen [S] with the module path [M] then you can produce:
   [S with type t = M.t]

   The compiler doesn't do this because it doesn't actually have a representation for `with` in its type algebra
   (`with` is always just expanded away right after parsing). But since we do have a representation for it,
   this is probably the best thing to produce in this case.
*)

open Component

let rec signature :
    Cpath.module_ ->
    ?canonical:Odoc_model.Paths.Path.Module.t ->
    Signature.t Delayed.t ->
    Signature.t Delayed.t =
 fun prefix ?canonical dsg ->
  let sg = Component.dget dsg in
  let sg', strengthened_modules = sig_items prefix ?canonical sg in
  let substs =
    List.fold_left
      (fun s mid -> Subst.path_invalidate_module (mid :> Ident.path_module) s)
      Subst.identity strengthened_modules
  in
  Subst.signature substs sg'

and sig_items prefix ?canonical sg =
  let open Signature in
  let items, ids =
    List.fold_left
      (fun (items, s) item ->
        match item with
        | Module (id, r, m) ->
            let name = Ident.Name.module_ id in
            let canonical =
              match canonical with
              | Some p -> Some (`Dot (p, name))
              | None -> None
            in
            let m' = module_ ?canonical (`Dot (prefix, name)) m in
            (Module (id, r, m') :: items, id :: s)
        | ModuleType (id, mt) ->
            ( ModuleType
                (id, module_type (`Dot (prefix, Ident.Name.module_type id)) mt)
              :: items,
              s )
        | Type (id, r, t) ->
            ( Type (id, r, type_decl (`Dot (prefix, Ident.Name.type_ id)) t)
              :: items,
              s )
        | Include i ->
            let i', strengthened = include_ prefix i in
            (Include i' :: items, strengthened @ s)
        | Exception _ | TypExt _ | Value _ | Class _ | ClassType _
        | ModuleSubstitution _ | TypeSubstitution _ | ModuleTypeSubstitution _
        | Comment _ | Open _ ->
            (item :: items, s))
      ([], []) sg.items
  in
  (Val { sg with items = List.rev items }, ids)

and module_ :
    ?canonical:Odoc_model.Paths.Path.Module.t ->
    Cpath.module_ ->
    Component.Module.t Delayed.t ->
    Component.Module.t Delayed.t =
 fun ?canonical prefix m ->
  let doc = Dhelpers.Module.doc m in
  let hidden = Dhelpers.Module.hidden m in
  Delayed.Val
    Component.Module.{ canonical; type_ = Alias (prefix, None); doc; hidden }

(* nuke the expansion as this could otherwise lead to inconsistencies - e.g. 'AlreadyASig' *)
and module_type :
    Cpath.module_type ->
    Component.ModuleType.t Delayed.t ->
    Component.ModuleType.t Delayed.t =
 fun prefix m ->
  let doc = Dhelpers.ModuleType.doc m in
  let canonical = Dhelpers.ModuleType.canonical m in
  let expr = Some (ModuleType.Path { p_path = prefix; p_expansion = None }) in
  Delayed.Val { doc; canonical; expr }

and type_decl : Cpath.type_ -> TypeDecl.t Delayed.t -> TypeDecl.t Delayed.t =
 fun path t ->
  let t = Component.dget t in
  let equation =
    let e = t.TypeDecl.equation in
    let open TypeDecl.Equation in
    let constr_params =
      List.map
        (fun { Odoc_model.Lang.TypeDecl.desc; _ } ->
          match desc with
          | Odoc_model.Lang.TypeDecl.Var x -> TypeExpr.Var x
          | Any -> Any)
        e.params
    in
    let manifest =
      match e.manifest with
      | None -> Some (TypeExpr.Constr (path, constr_params))
      | _ -> e.manifest
    in
    {
      params = e.params;
      private_ = e.private_;
      manifest;
      constraints = e.constraints;
    }
  in
  Delayed.Val { t with equation }

and include_ : Cpath.module_ -> Include.t -> Include.t * Ident.module_ list =
 fun path i ->
  let expansion_, strengthened = sig_items path i.expansion_ in
  ({ i with expansion_ = Component.dget expansion_; strengthened = Some path }, strengthened)
