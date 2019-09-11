open Odoc_model.Paths
open Component

type maps =
  { module_ : (Identifier.Module.t * Ident.t) list
  ; module_type : (Identifier.ModuleType.t * Ident.t) list
  ; type_ : (Identifier.Type.t * Ident.t) list
  ; exception_ : (Identifier.Exception.t * Ident.t) list
  ; extension : (Identifier.Extension.t * Ident.t) list
  ; value_ : (Identifier.Value.t * Ident.t) list
  ; field_ : (Identifier.Field.t * Ident.t) list
  ; constructor : (Identifier.Constructor.t * Ident.t) list
  ; class_ : (Identifier.Class.t * Ident.t) list
  ; class_type : (Identifier.ClassType.t * Ident.t) list
  ; method_ : (Identifier.Method.t * Ident.t) list
  ; instance_variable : (Identifier.InstanceVariable.t * Ident.t) list
  ; all : (Identifier.t * Ident.t) list
  }

let empty =
  { module_ = []
  ; module_type = []
  ; type_ = []
  ; exception_ = []
  ; extension = []
  ; value_ = []
  ; field_ = []
  ; constructor = []
  ; class_ = []
  ; class_type = []
  ; method_ = []
  ; instance_variable = []
  ; all = []
  }

let local_ids map sg =

  let module_ id map =
      let identifier = Ident.local_of_identifier (id :> Identifier.t) in 
      { map with module_ = (id, identifier)::map.module_
      ; all = ((id :> Identifier.t), identifier)::map.all }
  in
  let module_type id map =
      let identifier = Ident.local_of_identifier (id :> Identifier.t)  in 
      { map with module_type = (id, identifier)::map.module_type
      ; all = ((id :> Identifier.t), identifier)::map.all }
      in

  let type_ id map =
      let identifier =  Ident.local_of_identifier (id :> Identifier.t)  in
      { map with type_ = (id, identifier)::map.type_
      ; all = ((id :> Identifier.t), identifier)::map.all }
      in

  let exception_ id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with exception_ = (id, ident)::map.exception_ 
      ; all = ((id :> Identifier.t), ident)::map.all }
      in
  
  let extension id map =
      let ident =  Ident.local_of_identifier (id :> Identifier.t) in
      { map with extension = (id, ident)::map.extension 
      ; all = ((id :> Identifier.t), ident)::map.all }
      in
  
  let value_ id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with value_ = (id, ident)::map.value_ 
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let field_ id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with field_ = (id, ident)::map.field_
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let constructor id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with constructor = (id, ident)::map.constructor
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let class_ id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with class_ = (id, ident)::map.class_
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let class_type id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with class_type = (id, ident)::map.class_type
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let method_ id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with method_ = (id, ident)::map.method_
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let instance_variable id map =
      let ident = Ident.local_of_identifier (id :> Identifier.t) in
      { map with instance_variable = (id, ident)::map.instance_variable
      ; all = ((id :> Identifier.t), ident)::map.all }
      in

  let fns = {
      Func.Lang.module_; module_type; type_; exception_; extension; value_; field_; constructor; class_; class_type; method_; instance_variable }
      in

  Func.Lang.fold fns sg map


let ident_of_identifier ident_map identifier =
    List.assoc_opt identifier ident_map

let option conv ident_map x =
    match x with
    | None -> None
    | Some x' -> Some (conv ident_map x')

let rec local_resolved_path_of_resolved_path : maps -> Odoc_model.Paths.Path.Resolved.t -> Cpath.resolved = fun ident_map path ->
    let recurse p = local_resolved_path_of_resolved_path ident_map (p :> Odoc_model.Paths.Path.Resolved.t) in
    match path with
    | `Identifier i -> begin
        match ident_of_identifier ident_map.all i with
        | Some ident ->
            `Ident ident
        | None ->
            `Ident (`Identifier i)
        end
    | `Module (p, name) ->
        `Module (recurse p, name)
    | `ModuleType (p, name) ->
        `ModuleType (recurse p, name)
    | `Type (p, name) ->
        `Type (recurse p, name)
    | `Apply (p1, p2) ->
        `Apply (recurse p1, local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t))
    | `Alias (p1, p2) ->
        `Alias (recurse p1, recurse p2)
    | `Subst (p1, p2) ->
        `Subst (recurse p1, recurse p2)
    | `SubstAlias (p1, p2) ->
        `SubstAlias (recurse p1, recurse p2)
    | `Canonical (p1, p2) ->
        `Canonical (recurse p1, local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t))
    | `Hidden p1 ->
        `Hidden (recurse p1)
    | _ -> failwith "local_resolved_path_of_resolved_path"

and local_path_of_path : _ -> Odoc_model.Paths.Path.t -> Cpath.t = fun ident_map path ->
    match path with
    | `Resolved r ->
        `Resolved (local_resolved_path_of_resolved_path ident_map r)
    | `Dot (path', x) ->
        `Dot (local_path_of_path ident_map (path' :> Odoc_model.Paths.Path.t), x)
    | `Apply (p1, p2) ->
        `Apply (local_path_of_path ident_map (p1 :> Odoc_model.Paths.Path.t),
                (local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t)))
    | `Forward str ->
        `Forward str
    | `Root str ->
        `Root str

let rec type_decl ident_map id ty =
    let open Odoc_model.Lang.TypeDecl in
    { TypeDecl.id
    ; doc = ty.doc
    ; equation = type_equation ident_map ty.equation
    ; representation = Opt.map (type_decl_representation ident_map) ty.representation }

and type_decl_representation ident_map r =
    let open Odoc_model.Lang.TypeDecl.Representation in
    match r with
    | Variant v -> Variant (List.map (type_decl_constructor ident_map) v)
    | Record fs -> Record (List.map (type_decl_field ident_map) fs)
    | Extensible -> Extensible

and type_decl_field ident_map r =
    let open Odoc_model.Lang.TypeDecl.Field in
    let id = List.assoc r.id ident_map.field_ in
    { TypeDecl.Field.id
    ; doc = r.doc
    ; mutable_ = r.mutable_
    ; type_ = type_expression ident_map r.type_ }

and type_decl_constructor ident_map r =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    let id = List.assoc r.id ident_map.constructor in
    let args = function
    | Tuple ts -> TypeDecl.Constructor.Tuple (List.map (type_expression ident_map) ts)
    | Record fs -> TypeDecl.Constructor.Record (List.map (type_decl_field ident_map) fs)
    in
    { TypeDecl.Constructor.id
    ; doc = r.doc
    ; args = args r.args
    ; res = Opt.map (type_expression ident_map) r.res }

and type_equation ident_map teq =
    let open Odoc_model.Lang.TypeDecl.Equation in
    { TypeDecl.Equation.params = teq.params
    ; private_ = teq.private_
    ; manifest = option type_expression ident_map teq.manifest
    ; constraints = List.map (fun (x, y) -> (type_expression ident_map x, type_expression ident_map y)) teq.constraints }

and type_expr_polyvar ident_map v =
    let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
    let map_element = function
    | Type expr -> TypeExpr.Polymorphic_variant.Type (type_expression ident_map expr)
    | Constructor c ->
        Constructor TypeExpr.Polymorphic_variant.Constructor.{
            name = c.name;
            constant = c.constant;
            arguments = List.map (type_expression ident_map) c.arguments;
            doc = c.doc
        }
    in
    { TypeExpr.Polymorphic_variant.kind = v.kind
    ; elements = List.map map_element v.elements}

and type_object ident_map o =
    let open Odoc_model.Lang.TypeExpr.Object in
    let map_field = function
    | Method m -> TypeExpr.(Object.Method {Object.name=m.name; type_ = type_expression ident_map m.type_})
    | Inherit i -> Inherit (type_expression ident_map i)
    in
    { TypeExpr.Object.open_ = o.open_
    ; fields = List.map map_field o.fields}

and type_package ident_map pkg =
    let open Odoc_model.Lang.TypeExpr.Package in
    { TypeExpr.Package.path = local_path_of_path ident_map (pkg.path :> Odoc_model.Paths.Path.t)
    ; substitutions = List.map (fun (x,y) -> (x, type_expression ident_map y)) pkg.substitutions}

and type_expression ident_map expr =
    let open Odoc_model.Lang.TypeExpr in
    match expr with
    | Var s -> TypeExpr.Var s
    | Any -> Any
    | Constr (path, _) -> Constr (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t), [])
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expression ident_map t1, type_expression ident_map t2)
    | Tuple ts -> Tuple (List.map (type_expression ident_map) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_expr_polyvar ident_map v)
    | Poly (s,ts) -> Poly (s, type_expression ident_map ts)
    | Alias (t, s) -> Alias (type_expression ident_map t, s) 
    | Class (p, ts) -> Class (local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t), List.map (type_expression ident_map) ts)
    | Object o -> Object (type_object ident_map o)
    | Package p -> Package (type_package ident_map p)

and class_ ident_map id c =
    let open Odoc_model.Lang.Class in
    let rec decl = function
      | ClassType expr -> Class.ClassType (class_type_expr ident_map expr)
      | Arrow (lbl, t, d) -> Arrow (lbl, type_expression ident_map t, decl d)
    in
    { Class.id 
    ; doc = c.doc
    ; virtual_ = c.virtual_
    ; params = c.params
    ; type_ = decl c.type_
    }

and class_type_expr ident_map = function
    | Odoc_model.Lang.ClassType.Constr (path, ts) ->
        ClassType.Constr (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t),
            List.map (type_expression ident_map) ts)
    | Signature c -> Signature (class_signature ident_map c)

and class_type ident_map id c =
    let open Odoc_model.Lang.ClassType in
    { ClassType.id
    ; doc = c.doc
    ; virtual_ = c.virtual_
    ; params = c.params
    ; expr = class_type_expr ident_map c.expr }

and class_signature_item ident_map : Odoc_model.Lang.ClassSignature.item -> ClassSignature.item =
    function
    | Method m -> Method (method_ ident_map m)
    | InstanceVariable v -> InstanceVariable (instance_variable ident_map v)
    | Constraint (t1, t2) -> Constraint (type_expression ident_map t1, type_expression ident_map t2)
    | Inherit c -> Inherit (class_type_expr ident_map c)
    | Comment c -> Comment c

and class_signature ident_map c =
    let open Odoc_model.Lang.ClassSignature in
    { ClassSignature.self = Opt.map (type_expression ident_map) c.self
    ; items = List.map (class_signature_item ident_map) c.items }

and method_ ident_map m =
    let open Odoc_model.Lang.Method in
    let id = List.assoc m.id ident_map.method_ in
    { Method.id
    ; doc = m.doc
    ; private_ = m.private_
    ; virtual_ = m.virtual_
    ; type_ = type_expression ident_map m.type_ }

and instance_variable ident_map m =
    let open Odoc_model.Lang.InstanceVariable in
    let id = List.assoc m.id ident_map.instance_variable in
    { InstanceVariable.id
    ; doc = m.doc
    ; mutable_ = m.mutable_
    ; virtual_ = m.virtual_
    ; type_ = type_expression ident_map m.type_ }


and module_decl ident_map m =
    match m with
    | Odoc_model.Lang.Module.Alias p ->
        Module.Alias (local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t))
    | Odoc_model.Lang.Module.ModuleType s ->
        Module.ModuleType (module_type_expr ident_map s)

and canonical ident_map ( canonical : (Odoc_model.Paths.Path.Module.t * Odoc_model.Paths.Reference.Module.t) option) =
    match canonical with
    | Some (path, r) -> Some (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t), r)
    | None -> None

and module_ ident_map id m =
    let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
    let canonical = canonical ident_map m.Odoc_model.Lang.Module.canonical in
    let display_type = Opt.map (module_decl ident_map) m.Odoc_model.Lang.Module.display_type in
    {Module.id; doc = m.doc; type_; canonical; hidden=m.hidden; display_type}

and module_type_substitution ident_map m =
    let open Odoc_model.Lang.ModuleType in
    match m with
    | ModuleEq (frag, decl) ->
        ModuleType.ModuleEq (frag, module_decl ident_map decl)
    | ModuleSubst (frag, path) ->
        ModuleType.ModuleSubst (frag, local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t))
    | TypeEq (frag, eqn) ->
        ModuleType.TypeEq (frag, type_equation ident_map eqn)
    | TypeSubst (frag, eqn) ->
        ModuleType.TypeSubst (frag, type_equation ident_map eqn)

and functor_argument ident_map id a =
    let expr' = module_type_expr ident_map a.Odoc_model.Lang.FunctorArgument.expr in
    { FunctorArgument.id
    ; expr = expr' }

and module_type_expr ident_map m =
    match m with
    | Odoc_model.Lang.ModuleType.Signature s ->
        let s = signature ident_map s in
        ModuleType.Signature s
    | Odoc_model.Lang.ModuleType.Path p ->
        let p' = local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t) in
        ModuleType.Path p'
    | Odoc_model.Lang.ModuleType.With (e, subs) ->
        ModuleType.With (module_type_expr ident_map e,
            List.map (module_type_substitution ident_map) subs)
    | Odoc_model.Lang.ModuleType.Functor (Some arg, expr) ->
        let open Odoc_model.Paths in
        let identifier = arg.Odoc_model.Lang.FunctorArgument.id in
        let id = Ident.local_of_identifier (identifier :> Identifier.t) in
        let ident_map' =
            { ident_map with module_ = (identifier, id) :: ident_map.module_ 
            ; all = ((identifier :> Identifier.t), id)::ident_map.all }
        in
        let arg' = functor_argument ident_map' id arg in
        let expr' = module_type_expr ident_map' expr in
        ModuleType.Functor (Some arg', expr')
    | Odoc_model.Lang.ModuleType.Functor (None, expr) ->
        let expr' = module_type_expr ident_map expr in
        ModuleType.Functor (None, expr')
    | Odoc_model.Lang.ModuleType.TypeOf decl ->
        let decl' = module_decl ident_map decl in
        ModuleType.TypeOf decl'


and module_type ident_map id m =
    let expr = Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr in
    {ModuleType.id; doc = m.doc; expr}

and value ident_map id v =
    let type_ = type_expression ident_map v.Odoc_model.Lang.Value.type_ in
    { Value.id
    ; type_ 
    ; doc = v.doc }

and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
    fun ident_map items ->
        (* First we construct a list of brand new [Ident.t]s
            for each item in the signature *)
        let ident_map = local_ids ident_map items in
        (* Now we construct the Components for each item,
            converting all paths containing Identifiers pointing at
            our elements to local paths *)
        let items = List.map (
            let open Odoc_model.Lang.Signature in
            function
            |  Type (r, t) ->
                let id = List.assoc t.id ident_map.type_ in 
                let t' = type_decl ident_map id t in
                Signature.Type (r,t')
            | Module (r, m) ->
                let id = List.assoc m.id ident_map.module_ in 
                let m' = module_ ident_map id m in
                Signature.Module (r,m')
            | ModuleType m ->
                let id = List.assoc m.id ident_map.module_type in 
                let m' = module_type ident_map id m in
                Signature.ModuleType m'
            | Value v ->
                let id = List.assoc v.id ident_map.value_ in
                let v' = value ident_map id v in
                Signature.Value v'
            | Comment c ->
                Signature.Comment c
            | TypExt _ ->
                failwith "Unhandled typext in of_signature"
            | Exception _ ->
                failwith "Unhandled exception in of_signature"
            | External _ ->
                failwith "Unhandled external in of_signature"
            | Class (r,c) ->
                let id = List.assoc c.id ident_map.class_ in
                let c' = class_ ident_map id c in
                Signature.Class (r, c')
            | ClassType (r,c) ->
                let id = List.assoc c.id ident_map.class_type in
                let c' = class_type ident_map id c in
                Signature.ClassType (r, c')
            | Include _ ->
                failwith "Unhandled include in of_signature"
            ) items
        in
        { items; removed=[] }
