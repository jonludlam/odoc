(* A bunch of association lists. Let's hashtbl them up later *)
type t =
    { ident_max: int
    ; modules : (Odoc_model.Paths.Identifier.Module.t * Component.Module.t) list
    ; module_types : (Odoc_model.Paths.Identifier.ModuleType.t * Component.ModuleType.t) list
    ; types : (Odoc_model.Paths.Identifier.Type.t * Component.TypeDecl.t) list }

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
    ; types = [] }

let add_module identifier m env =
    { env with modules = (identifier, m)::env.modules}

let add_type identifier t env =
    { env with types = (identifier, t)::env.types}

let add_module_type identifier t env =
    { env with module_types = (identifier, t)::env.module_types}

let lookup_module identifier env =
    try
        List.assoc identifier env.modules
    with _ -> raise (MyFailure ((identifier :> Odoc_model.Paths.Identifier.t), env))

let lookup_type identifier env =
    try
        List.assoc identifier env.types
    with Not_found ->
        Format.fprintf Format.std_formatter "Failed to find type:\nIdentifier: %a\n\nEnv:\n%a\n\n%!" Component.Fmt.model_identifier (identifier :> Odoc_model.Paths.Identifier.t) pp env;
        raise Not_found

let lookup_module_type identifier env =
    List.assoc identifier env.module_types

let add_functor_args : Odoc_model.Paths.Identifier.Signature.t -> t -> t =
    let open Component in
    fun id env ->
        let rec find_args parent mty =
            match mty with 
            | ModuleType.Functor (Some arg, res) ->
                (`Parameter (parent, Odoc_model.Names.ParameterName.of_string (Ident.name arg.Component.FunctorArgument.id)),
                    {Component.Module.id = arg.Component.FunctorArgument.id; type_ = ModuleType arg.expr; canonical=None; hidden=false}) :: find_args (`Result parent) res
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
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.type_decl [identifier,id] id t in
                add_type t.Odoc_model.Lang.TypeDecl.id ty env
            | Odoc_model.Lang.Signature.Module (_, t) ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.module_ [identifier,id] id t in
                add_module t.Odoc_model.Lang.Module.id ty env
            | Odoc_model.Lang.Signature.ModuleType t ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.module_type [identifier,id] id t in
                add_module_type t.Odoc_model.Lang.ModuleType.id ty env
            | Odoc_model.Lang.Signature.Comment _ ->
                Printf.fprintf stderr "open_signature: Comment\n%!";
                env
            | Odoc_model.Lang.Signature.TypExt _ ->
                Printf.fprintf stderr "open_signature: TypExt\n%!";
                env
            | Odoc_model.Lang.Signature.Exception _ ->
                Printf.fprintf stderr "open_signature: Exception\n%!";
                env
            | Odoc_model.Lang.Signature.Value _ ->
                Printf.fprintf stderr "open_signature: Value\n%!";
                env
            | Odoc_model.Lang.Signature.External _ ->
                Printf.fprintf stderr "open_signature: External\n%!";
                env
            | Odoc_model.Lang.Signature.Class _ ->
                Printf.fprintf stderr "open_signature: Class\n%!";
                env
            | Odoc_model.Lang.Signature.ClassType _ ->
                Printf.fprintf stderr "open_signature: ClassType\n%!";
                env
            | Odoc_model.Lang.Signature.Include _ ->
                Printf.fprintf stderr "open_signature: Include\n%!";
                env) env s

let open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t =
    fun unit env ->
        match unit.content with
        | Module s -> open_signature s env
        | Pack _ -> env

