open Odoc_model
open Lang


type expander =
  { lookup_unit: string -> Tools.lookup_unit_result
  ; resolve_unit: Root.t -> Compilation_unit.t
  }

module Lang_of = struct
  open Odoc_model.Paths

  type maps =
    { module_ : (Ident.t * Identifier.Module.t) list
    ; module_type : (Ident.t * Identifier.ModuleType.t) list
    ; type_ : (Ident.t * Identifier.Type.t) list
    ; path_type : (Ident.t * Odoc_model.Paths_types.Identifier.path_type) list
    ; exception_ : (Ident.t * Identifier.Exception.t) list
    ; extension : (Ident.t * Identifier.Extension.t) list
    ; value_ : (Ident.t * Identifier.Value.t) list
    ; field_ : (Ident.t * Identifier.Field.t) list
  }

  let empty =
    { module_ = []
    ; module_type = []
    ; type_ = []
    ; path_type = []
    ; exception_ = []
    ; extension = []
    ; value_ = []
    ; field_ = []
    }

  module Opt = Component.Opt

  module Path = struct
    let rec module_ map (p : Cpath.t) : Odoc_model.Paths.Path.Module.t =
      match p with
      | `Substituted x -> module_ map x
      | `Resolved x -> `Resolved (resolved_module map x)
      | `Root x -> `Root x
      | `Dot (p, s) -> `Dot (module_ map p, s)
      | `Forward s -> `Forward s
      | `Apply (m1, m2) -> `Apply (module_ map m1, module_ map m2)
    
    and module_type map (p : Cpath.t) : Odoc_model.Paths.Path.ModuleType.t =
      match p with
      | `Substituted x -> module_type map x
      | `Resolved x -> `Resolved (resolved_module_type map x)
      | `Dot (p, n) -> `Dot (module_ map p, n)
      | `Root _
      | `Forward _
      | `Apply _ -> failwith "type error"

    and type_ map (p : Cpath.t) : Odoc_model.Paths.Path.Type.t =
      match p with
      | `Substituted x -> type_ map x
      | `Resolved x -> `Resolved (resolved_type map x)
      | `Dot (p, n) -> `Dot (module_ map p, n)
      | `Root _
      | `Forward _
      | `Apply _ -> failwith "type error"
    
    and resolved_module map (p : Cpath.resolved) : Odoc_model.Paths.Path.Resolved.Module.t =
      match p with
      | `Local id ->
        `Identifier (List.assoc id map.module_)
      | `Substituted x -> resolved_module map x
      | `Identifier (#Odoc_model.Paths.Identifier.Module.t as y) -> `Identifier y
      | `Subst (mty, m) -> `Subst (resolved_module_type map mty, resolved_module map m)
      | `SubstAlias (m1, m2) -> `SubstAlias (resolved_module map m1, resolved_module map m2)
      | `Hidden h -> `Hidden (resolved_module map h)
      | `Module (p, n) -> `Module (resolved_module map p, n)
      | `Canonical (r, m) -> `Canonical (resolved_module map r, module_ map m)
      | `Apply (m1, m2) -> `Apply (resolved_module map m1, module_ map m2)
      | `Alias (m1, m2) -> `Alias (resolved_module map m1, resolved_module map m2)
      | `Identifier _
      | `ModuleType (_,_)
      | `Type (_,_) -> failwith "type error"

    and resolved_module_type map (p : Cpath.resolved) : Odoc_model.Paths.Path.Resolved.ModuleType.t =
      match p with
      | `Identifier (#Odoc_model.Paths.Identifier.ModuleType.t as y) -> `Identifier y
      | `Local id -> `Identifier (List.assoc id map.module_type)
      | `ModuleType (p, name) -> `ModuleType (resolved_module map p, name)
      | _ -> failwith "type error"
    
    and resolved_type map (p : Cpath.resolved) : Odoc_model.Paths.Path.Resolved.Type.t =
      match p with
      | `Identifier (#Odoc_model.Paths.Identifier.Type.t as y) -> `Identifier y
      | `Local id -> `Identifier (List.assoc id map.path_type)
      | `Type (p, name) -> `Type (resolved_module map p, name)
      | _ -> failwith "type error"

  end

  module ExtractIDs = struct
    open Component
    open Odoc_model.Names

    let rec exception_ parent map e =
      let open Exception in
      let identifier = `Exception (parent, ExceptionName.of_string (Ident.name e.id)) in
      let map = type_decl_constructor_argument (parent :> Odoc_model.Paths.Identifier.Parent.t) map e.args in
      {map with exception_ = (e.id, identifier)::map.exception_ }

    and extension parent map e =
      let open Extension in
      let constructor map c =
        let id = `Extension (parent, Ident.name c.Constructor.id) in
        let map = {map with extension = (c.Constructor.id, id)::map.extension } in
        type_decl_constructor_argument (parent :> Odoc_model.Paths.Identifier.Parent.t) map c.Constructor.args
      in
      List.fold_left constructor map e.constructors

    and type_decl_constructor_argument parent map a =
      let open TypeDecl.Constructor in
      match a with
      | Tuple _ -> map
      | Record fs -> List.fold_left (type_decl_field parent) map fs
    
    and type_decl_field (parent : Odoc_model.Paths.Identifier.Parent.t) map f =
      let open TypeDecl.Field in
      let identifier = `Field (parent, FieldName.of_string (Ident.name f.id)) in
      { map with field_ = (f.id, identifier)::map.field_ }

    and type_decl parent map t =
      let open TypeDecl in
      let identifier = `Type (parent, TypeName.of_string (Ident.name t.id)) in
      { map with type_ = (t.id, identifier)::map.type_
      ; path_type = (t.id, identifier)::map.path_type }

    and module_ parent map m =
      let identifier = `Module (parent, ModuleName.of_string (Ident.name m.Module.id)) in 
      { map with module_ = (m.Module.id, identifier)::map.module_}

    and module_type parent map m =
      let identifier = `ModuleType (parent, ModuleTypeName.of_string (Ident.name m.ModuleType.id)) in 
      { map with module_type = (m.ModuleType.id, identifier)::map.module_type}

    and value_ parent map v =
      let identifier = `Value (parent, ValueName.of_string (Ident.name v.Value.id)) in
      {map with value_ = (v.Value.id, identifier)::map.value_ }

    and signature parent map sg =
      let open Signature in
      List.fold_left (fun map item ->
        match item with
        | Module (_, m) -> module_ parent map m
        | ModuleType m -> module_type parent map m
        | Type (_, t) -> type_decl parent map t
        | Exception e -> exception_ parent map e          
        | TypExt e -> extension parent map e
        | Value v -> value_ parent map v
        | Comment _ -> map
        ) map sg.items

  end


  let rec signature id map sg =
    let open Component.Signature in

    let map = ExtractIDs.signature id map sg in
    List.fold_right (fun item acc ->
      match item with
      | Module (r, m) ->
        Odoc_model.Lang.Signature.Module (r, module_ map m) :: acc 
      | ModuleType m ->
        Odoc_model.Lang.Signature.ModuleType (module_type map m) :: acc
      | Type (r, t) ->
        Odoc_model.Lang.Signature.Type (r, type_decl map t) :: acc
      | Exception e ->
        Odoc_model.Lang.Signature.Exception (exception_ map e) :: acc
      | TypExt t ->
        Odoc_model.Lang.Signature.TypExt (typ_ext map t) :: acc
      | Value v ->
        Odoc_model.Lang.Signature.Value (value_ map v) :: acc
      | Comment c ->
        Odoc_model.Lang.Signature.Comment c :: acc
    ) sg.items []

  and value_ map v =
    let open Component.Value in
    let identifier = List.assoc v.id map.value_ in
    { id = identifier
    ; doc = v.doc
    ; type_ = type_expr map v.type_ }

  and typ_ext map t =
    let open Component.Extension in
    { type_path = Path.type_ map t.type_path
    ; doc = t.doc
    ; type_params = t.type_params
    ; private_ = t.private_
    ; constructors = List.map (extension_constructor map) t.constructors }
  
  and extension_constructor map c =
    let open Component.Extension.Constructor in
    let identifier = List.assoc c.id map.extension in
    { id = identifier
    ; doc = c.doc
    ; args = type_decl_constructor_argument map c.args
    ; res = Opt.map (type_expr map) c.res }

  and module_ map m =
    let open Component.Module in
    let identifier = (List.assoc m.id map.module_ :> Odoc_model.Paths_types.Identifier.signature) in
    let canonical = function
      | Some (p, r) -> Some (Path.module_ map p, r)
      | None -> None
    in
    { Odoc_model.Lang.Module.id = List.assoc m.id map.module_
    ; doc = m.doc
    ; type_ = module_decl map (identifier :> Odoc_model.Paths_types.Identifier.signature) m.type_ 
    ; canonical = canonical m.canonical
    ; hidden = m.hidden
    ; display_type = Opt.map (module_decl map identifier) m.display_type
    ; expansion = None }
  
  and module_decl map (identifier : Odoc_model.Paths_types.Identifier.signature) = function
    | Component.Module.Alias p -> Odoc_model.Lang.Module.Alias (Path.module_ map p)
    | ModuleType mty -> ModuleType (module_type_expr map identifier mty)
  
  and module_type_expr map identifier =
    let substitution = function
      | Component.ModuleType.ModuleEq (frag, decl) -> Odoc_model.Lang.ModuleType.ModuleEq (frag, module_decl map identifier decl)
      | ModuleSubst (frag, path) -> ModuleSubst (frag, Path.module_ map path)
      | TypeEq (frag, eqn) -> TypeEq (frag, type_decl_equation map eqn)
      | TypeSubst (frag, eqn) -> TypeSubst (frag, type_decl_equation map eqn)
    in
    function
    | Component.ModuleType.Path p -> Odoc_model.Lang.ModuleType.Path (Path.module_type map p)
    | Signature s -> Signature (signature (identifier :> Odoc_model.Paths.Identifier.Signature.t) map s)
    | With (expr, subs) -> With (module_type_expr map identifier expr, List.map substitution subs)
    | Functor (Some arg, expr) ->
        let identifier = `Parameter (identifier, Odoc_model.Names.ParameterName.of_string (Ident.name arg.id)) in 
        let map = { map with module_ = (arg.id, identifier) :: map.module_ } in
        Functor (Some (functor_argument map arg), module_type_expr map (`Result identifier) expr)
    | Functor (None, expr) -> 
      Functor (None, module_type_expr map (`Result identifier) expr)
    | TypeOf decl -> TypeOf (module_decl map identifier decl)

  and module_type map mty =
    let identifier = List.assoc mty.id map.module_type in
    let sig_id = (identifier :> Odoc_model.Paths.Identifier.Signature.t) in
    { Odoc_model.Lang.ModuleType.id = identifier
    ; doc = mty.doc
    ; expr = Opt.map (module_type_expr map sig_id) mty.expr
    ; expansion = None}
  
  and type_decl_constructor_argument map (a : Component.TypeDecl.Constructor.argument) : Odoc_model.Lang.TypeDecl.Constructor.argument =
    match a with
    | Tuple ls -> Tuple (List.map (type_expr map) ls)
    | Record fs -> Record (List.map (type_decl_field map) fs)
  
  and type_decl_field map (f : Component.TypeDecl.Field.t) : Odoc_model.Lang.TypeDecl.Field.t =
    let identifier = List.assoc f.id map.field_ in
    { id = identifier
    ; doc =  f.doc
    ; mutable_ = f.mutable_ 
    ; type_ = type_expr map f.type_}
  
  and type_decl_equation map (eqn : Component.TypeDecl.Equation.t) : Odoc_model.Lang.TypeDecl.Equation.t =
    { params = eqn.params
    ; private_ = eqn.private_
    ; manifest = Opt.map (type_expr map) eqn.manifest
    ; constraints = List.map (fun (x, y) -> (type_expr map x, type_expr map y)) eqn.constraints
    }

  and type_decl map (t : Component.TypeDecl.t) : Odoc_model.Lang.TypeDecl.t =
    let identifier = List.assoc t.id map.type_ in
    { id = identifier
    ; equation = type_decl_equation map t.equation
    ; doc = []
    ; representation = None }

  and type_expr map (t : Component.TypeExpr.t) : Odoc_model.Lang.TypeExpr.t =
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t, str) -> Alias (type_expr map t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr map t1, type_expr map t2)
    | Tuple ts -> Tuple (List.map (type_expr map) ts)
    | Constr (path, ts) -> Constr (Path.type_ map path, List.map (type_expr map) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_expr_polyvar map v)
    | Object o -> Object (type_expr_object map o)
    | Class (_p, _ts) -> failwith "Unimplemented"
    | Poly (strs, t) -> Poly (strs, type_expr map t)
    | Package _ -> failwith "Unimplemented"
  
  and type_expr_polyvar map v =
    let constructor c =
      { Lang.TypeExpr.Polymorphic_variant.Constructor.name = c.Component.TypeExpr.Polymorphic_variant.Constructor.name
      ; constant = c.constant
      ; arguments = List.map (type_expr map) c.arguments
      ; doc = c.doc}
    in
    let element = function
    | Component.TypeExpr.Polymorphic_variant.Type t -> Lang.TypeExpr.Polymorphic_variant.Type (type_expr map t)
    | Constructor c -> Constructor (constructor c)
    in
    { kind = v.kind
    ; elements = List.map element v.elements}

  and type_expr_object map o =
      let method_ m =
        { Lang.TypeExpr.Object.name = m.Component.TypeExpr.Object.name
        ; type_ = type_expr map m.type_ }
      in
      let field = function
        | Component.TypeExpr.Object.Method m -> Lang.TypeExpr.Object.Method (method_ m)
        | Inherit i -> Inherit (type_expr map i)
      in
      { Lang.TypeExpr.Object.fields = List.map field o.fields
      ; open_ = o.open_ }
  
  and functor_argument map f =
      let identifier = List.assoc f.id map.module_ in
      { Odoc_model.Lang.FunctorArgument.id = identifier
      ; expr = module_type_expr map (identifier :> Odoc_model.Paths_types.Identifier.signature) f.expr
      ; expansion = None }

  and exception_ map (e : Component.Exception.t) : Odoc_model.Lang.Exception.t =
      let identifier = List.assoc e.id map.exception_ in
      { id=identifier
      ; doc = e.doc
      ; args = type_decl_constructor_argument map e.args
      ; res = Opt.map (type_expr map) e.res }


    end

let rec unit expander t =
  let open Tools in
  let open Compilation_unit in
  let (imports, env) = List.fold_left (fun (imports,env) import ->
      match import with
      | Import.Resolved root ->
          let unit = expander.resolve_unit root in
          Printf.fprintf stderr "Import: found module %s\n%!" (Root.to_string root);
          let env = Env.add_unit unit env in
          (import::imports, env)
      | Import.Unresolved (str, _) ->
          match expander.lookup_unit str with
          | Forward_reference ->
(*                Printf.fprintf stderr "Import: forward reference %s\n%!" str;*)
              (import::imports, env)
          | Found f ->
              let unit = expander.resolve_unit f.root in
(*                Printf.fprintf stderr "Import: found module %s\n%!" (Root.to_string f.root);*)
              let env = Env.add_unit unit env in
              ((Resolved f.root)::imports, env)
          | Not_found ->
(*                Printf.fprintf stderr "Not found: %s\n%!" str;*)
              (import::imports,env)
  ) ([],Env.empty) t.imports in
  {t with content = content env t.content; imports}

and content env =
    let open Compilation_unit in
    function
    | Module m -> Module (signature env m)
    | Pack _ -> failwith "Unhandled content"

and signature : Env.t -> Signature.t -> _ = fun env s ->
  let open Signature in
  let env = Env.open_signature s env in
  let (_, items') = 
      List.fold_right (fun item (env, items) ->
          match item with
          | Module (r, m) ->
              let m' = module_ env m in
              (env, (Module (r, m'))::items)
          | ModuleType mt ->
              let mt' = module_type env mt in
              (env, (ModuleType mt')::items)
          | x -> (env, x::items)
        ) s (env, [])
  in items'

(*and expansion_of_signature env sg =
    let sg' = List.map
      (fun )
*)

and module_ env m =
    let open Module in
    match m.type_ with
    | Alias _ -> m
    | ModuleType _ -> begin
      let id = (m.id :> Odoc_model.Paths.Identifier.Signature.t) in
      match Tools.lookup_and_resolve_module_from_path false true env (`Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))) with
      | Ok (p, m') ->
        let (_, sg) = Tools.signature_of_module env (p, m') |> Tools.prefix_signature in
        let sg = Lang_of.signature id Lang_of.empty sg in
        {m with expansion=Some (Signature (signature env sg))}
      | _ ->
        m
    end




and module_type env m =
  let id = (m.id :> Odoc_model.Paths.Identifier.Signature.t) in
  match Tools.lookup_and_resolve_module_type_from_path false env (`Resolved (`Identifier (id :> Odoc_model.Paths.Identifier.t))) with
  | Ok (p, m') ->
    let (_, sg) = Tools.signature_of_module_type env (p, m') |> Tools.prefix_signature in
    let sg = Lang_of.signature id Lang_of.empty sg in
    {m with expansion=Some (Signature (signature env sg))}
  | _ ->
    m

  

let build_expander :
    ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
    -> (string -> Tools.lookup_unit_result)
    -> (Root.t -> Compilation_unit.t)
    -> expander =
    fun ?equal:_ ?hash:_ lookup_unit resolve_unit ->
    {lookup_unit; resolve_unit; }

let expand x y =
    let before = y in
    let after = unit x before in
    after

let resolve_page _ x = x
