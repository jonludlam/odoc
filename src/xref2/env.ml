(* A bunch of association lists. Let's hashtbl them up later *)

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type root =
    | Resolved of (Odoc_model.Paths.Identifier.Module.t * Component.Module.t)
    | Forward

type resolver =
    { lookup_unit: string -> lookup_unit_result
    ; resolve_unit: Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t
    ; lookup_page: string -> Odoc_model.Root.t option
    ; resolve_page: Odoc_model.Root.t -> Odoc_model.Lang.Page.t }

type t =
    { ident_max: int
    ; modules : (Odoc_model.Paths.Identifier.Module.t * Component.Module.t) list
    ; module_types : (Odoc_model.Paths.Identifier.ModuleType.t * Component.ModuleType.t) list
    ; types : (Odoc_model.Paths.Identifier.Type.t * Component.TypeDecl.t) list
    ; values : (Odoc_model.Paths.Identifier.Value.t * Component.Value.t) list
    ; titles : (Odoc_model.Paths.Identifier.Label.t * Odoc_model.Comment.link_content) list
    ; classes : (Odoc_model.Paths.Identifier.Class.t * Component.Class.t) list
    ; class_types : (Odoc_model.Paths.Identifier.ClassType.t * Component.ClassType.t) list
    ; elts : (string * Component.Element.any) list
    ; roots : (string * root) list
    ; resolver : resolver option
    }

let pp_modules ppf modules =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.module_ m) modules

let pp_module_types ppf module_types =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.module_type m) module_types

let pp_types ppf types =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.type_decl m) types

let pp ppf env =
    Format.fprintf ppf "@[<v>@,modules: %a @,module_types: %a @,types: %a@," pp_modules env.modules pp_module_types env.module_types pp_types env.types
(* Handy for extrating transient state *)
exception MyFailure of Odoc_model.Paths.Identifier.t * t

let empty =
    { ident_max = 0
    ; modules = []
    ; module_types = []
    ; types = []
    ; values = []
    ; titles = []
    ; elts = []
    ; roots = []
    ; classes = []
    ; class_types = []
    ; resolver = None
    }

let add_module identifier m env =
    { env with modules = (identifier, m)::env.modules
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `Module (identifier, m))::env.elts }

let add_type identifier t env =
    { env with types = (identifier, t)::env.types
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `Type (identifier, t))::env.elts }

let add_module_type identifier t env =
    { env with module_types = (identifier, t)::env.module_types
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `ModuleType (identifier, t))::env.elts }

let add_value identifier t env =
    { env with values = (identifier, t)::env.values
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `Value (identifier, t))::env.elts }

let add_label identifier env =
    { env with elts = (Odoc_model.Paths.Identifier.name identifier, `Label identifier)::env.elts }

let add_label_title label elts env =
    { env with titles = (label, elts) :: env.titles }

let add_class identifier t env =
    { env with classes = (identifier, t) :: env.classes
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `Class (identifier, t))::env.elts }

let add_class_type identifier t env =
    { env with class_types = (identifier, t) :: env.class_types
    ; elts = (Odoc_model.Paths.Identifier.name identifier, `ClassType (identifier, t))::env.elts }

let add_docs (docs : Component.Comment.docs) env =
    List.fold_right (fun element env ->
    match element.Odoc_model.Location_.value with
    | `Heading (_, label, nested_elements) ->
      let env = add_label label env in
      let env = add_label_title label nested_elements env in
      env
    | _ -> env)
    docs env

let add_comment (com : Odoc_model.Comment.docs_or_stop) env =
    match com with
    | `Docs doc -> add_docs doc env
    | `Stop -> env

let add_root name ty env =
    { env with roots = (name, ty)::env.roots }

let lookup_module identifier env =
    try
        List.assoc identifier env.modules
    with _ ->
        Format.fprintf Format.std_formatter "Failed to find module:\nIdentifier: %a\n\n" Component.Fmt.model_identifier (identifier :> Odoc_model.Paths.Identifier.t);
        raise (MyFailure ((identifier :> Odoc_model.Paths.Identifier.t), env))

let lookup_type identifier env =
    try
        List.assoc identifier env.types
    with Not_found ->
        Format.fprintf Format.std_formatter "Failed to find type:\nIdentifier: %a\n\nEnv:\n%a\n\n%!" Component.Fmt.model_identifier (identifier :> Odoc_model.Paths.Identifier.t) pp env;
        raise Not_found

let lookup_module_type identifier env =
    List.assoc identifier env.module_types

let lookup_value identifier env =
    List.assoc identifier env.values

let lookup_section_title identifier env =
    List.assoc_opt identifier env.titles

let lookup_class identifier env =
    List.assoc identifier env.classes

let lookup_class_type identifier env =
    List.assoc identifier env.class_types

let module_of_unit : Odoc_model.Lang.Compilation_unit.t -> Component.Module.t =
    fun unit ->
        match unit.content with
        | Module s ->
            let m = Odoc_model.Lang.Module.{
                id = unit.id;
                doc = unit.doc;
                type_ = ModuleType (Signature s);
                canonical = None;
                hidden = unit.hidden;
                display_type = None;
                expansion = Some AlreadyASig
            }
            in
            let identifier = (m.id :> Odoc_model.Paths.Identifier.t) in
            let id = Ident.of_identifier identifier in
            let ty = Component.Of_Lang.(module_ {empty with modules = [m.id,id]} m) in
            ty
       | Pack _ ->
            failwith "Unsupported"    

let lookup_root_module name env =
    match List.assoc_opt name env.roots with
    | Some x -> Some x 
    | None ->
        match env.resolver with
        | None -> None
        | Some r ->
            match r.lookup_unit name with
            | Forward_reference -> Some Forward
            | Not_found -> None
            | Found u ->
                let unit = r.resolve_unit u.root in
                Some (Resolved (unit.id, module_of_unit unit))

let find_map : ('a -> 'b option) -> 'a list -> 'b option = fun f -> 
    let rec inner acc = function
        | x::xs -> (match f x with | Some y -> Some y | None -> inner acc xs)
        | [] -> None
    in inner []

let lookup_any_by_name name env =
    let filter_fn : (string * Component.Element.any) -> Component.Element.any option = function
        | (n,(_ as item)) when n=name -> Some item
        | _ -> None
    in
    find_map filter_fn env.elts

let lookup_signature_by_name name env =
    let filter_fn : (string * Component.Element.any) -> Component.Element.signature option = function
        | (n,(#Component.Element.signature as item)) when n=name -> Some item
        | _ -> None
    in
    find_map filter_fn env.elts

let lookup_module_by_name name env =
    let filter_fn : (string * Component.Element.any) -> Component.Element.module_ option = function
        | (n,(#Component.Element.module_ as item)) when n=name -> Some item
        | _ -> None
    in
    find_map filter_fn env.elts

let lookup_value_by_name name env =
    let filter_fn : (string * Component.Element.any) -> Component.Element.value option = function
        | (n,(#Component.Element.value as item)) when n=name -> Some item
        | _ -> None
    in
    find_map filter_fn env.elts
    
let add_functor_args : Odoc_model.Paths.Identifier.Signature.t -> t -> t =
    let open Component in
    fun id env ->
        let rec find_args parent mty =
            match mty with 
            | ModuleType.Functor (Some arg, res) ->
                (`Parameter (parent, Odoc_model.Names.ParameterName.of_string (Ident.name arg.Component.FunctorArgument.id)),
                    {Component.Module.doc = []; display_type = None; type_ = ModuleType arg.expr; canonical=None; hidden=false}) :: find_args (`Result parent) res
            | ModuleType.Functor (None, res) ->
                find_args (`Result parent) res
            | _ -> []
        in
        match id with
        | `Module _
        | `Result _
        | `Parameter _ as mid -> begin
            let m = lookup_module mid env in
            match m.Component.Module.type_ with
            | Alias _ -> env
            | ModuleType e -> 
                List.fold_left (fun env (id,m) -> add_module id m env) env (find_args id e)
            end
        | `ModuleType _ as mtyid -> begin
            let m = lookup_module_type mtyid env in
            match m.Component.ModuleType.expr with
            | Some e ->
                List.fold_left (fun env (id,m) -> add_module id m env) env (find_args id e)
            | None ->
                env
            end
        | `Root _ -> env

let open_signature : Odoc_model.Lang.Signature.t -> t -> t =
    let open Component in
    fun s env ->
        List.fold_left (fun env orig ->
            match orig with
            | Odoc_model.Lang.Signature.Type (_, t) ->
                let idents = Component.LocalIdents.(type_decl t {empty with types = [t.id]}) in
                let map = Of_Lang.(map_of_idents idents empty) in
                let ty = Of_Lang.(type_decl map t) in
                add_type t.Odoc_model.Lang.TypeDecl.id ty env
            | Odoc_model.Lang.Signature.Module (_, t) ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.(module_ {empty with modules = [t.id,id]} t) in
                add_module t.Odoc_model.Lang.Module.id ty env
            | Odoc_model.Lang.Signature.ModuleType t ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.(module_type {empty with module_types = [t.id,id]} t) in
                add_module_type t.Odoc_model.Lang.ModuleType.id ty env
            | Odoc_model.Lang.Signature.Comment c ->
                add_comment c env
            | Odoc_model.Lang.Signature.TypExt _ ->
                env
            | Odoc_model.Lang.Signature.Exception _ ->
                env
            | Odoc_model.Lang.Signature.Value v ->
                let identifier = (v.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.(value {empty with values = [v.id,id]} v) in
                add_value v.Odoc_model.Lang.Value.id ty env
            | Odoc_model.Lang.Signature.External _ ->
                env
            | Odoc_model.Lang.Signature.Class (_,c) ->
                let identifier = (c.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.(class_ {empty with classes = [c.id,id]} c) in
                add_class c.id ty env
            | Odoc_model.Lang.Signature.ClassType (_,c) ->
                let identifier = (c.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.(class_type {empty with class_types = [c.id,id]} c) in
                add_class_type c.id ty env            
            | Odoc_model.Lang.Signature.Include _ ->
                env) env s

let open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t =
    fun unit env ->
        match unit.content with
        | Module s -> open_signature s env
        | Pack _ -> env
