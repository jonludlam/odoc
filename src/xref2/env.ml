(* A bunch of association lists. Let's hashtbl them up later *)
type t =
    { ident_max: int
    ; modules : (Odoc_model.Paths.Identifier.Module.t * Component.Module.t) list
    ; module_types : (Odoc_model.Paths.Identifier.ModuleType.t * Component.ModuleType.t) list
    ; types : (Odoc_model.Paths.Identifier.Type.t * Component.Type.t) list }

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
    List.assoc identifier env.types

let lookup_module_type identifier env =
    List.assoc identifier env.module_types

let open_signature : Odoc_model.Lang.Signature.t -> t -> t =
    let open Component in
    fun s env ->
        List.fold_left (fun env orig ->
            match orig with
            | Odoc_model.Lang.Signature.Type (_, t) ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.of_type [identifier,id] id t in
                add_type t.Odoc_model.Lang.TypeDecl.id ty env
            | Odoc_model.Lang.Signature.Module (_, t) ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.of_module [identifier,id] id t in
                add_module t.Odoc_model.Lang.Module.id ty env
            | Odoc_model.Lang.Signature.ModuleType t ->
                let identifier = (t.id :> Odoc_model.Paths.Identifier.t) in
                let id = Ident.of_identifier identifier in
                let ty = Of_Lang.of_module_type [identifier,id] id t in
                add_module_type t.Odoc_model.Lang.ModuleType.id ty env
            | _ -> failwith "foo") env s

let pp_modules ppf modules =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.module_ m) modules

let pp_module_types ppf module_types =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.module_type m) module_types

let pp_types ppf types =
    List.iter (fun (i,m) ->
        Format.fprintf ppf "%a: %a @," Component.Fmt.model_identifier (i :> Odoc_model.Paths.Identifier.t) Component.Fmt.type_ m) types

let pp ppf env =
    Format.fprintf ppf "@[<v>@,modules: %a @,module_types: %a @,types: %a@," pp_modules env.modules pp_module_types env.module_types pp_types env.types