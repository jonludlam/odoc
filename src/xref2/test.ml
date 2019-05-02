(* Playground for new model *)

let cmti_of_string s =
    let env = Compmisc.initial_env () in
    let l = Lexing.from_string s in
    let p = Parse.interface l in
    Typemod.type_interface "" env p;;

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Model.Root.Odoc_file.t =
  Model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Model.Root.package; file = file_representation; digest}

let dummy_root = 
    root_of_compilation_unit
        ~package:"nopackage"
        ~hidden:false
        ~module_name:"Test"
        ~digest:"nodigest"

let model_of_string str = 
    let cmti = cmti_of_string str in
    Odoc__loader__Cmti.read_interface dummy_root "noname" cmti

let myexample = model_of_string {|
module X : sig
  type t

  type u = t
end

type x = X.t
|}

module Component = struct
    type t 
end

type env = (Model.Paths.Identifier.t * Component.t) list



let rec resolve_unit env t =
    let open Model.Lang.Compilation_unit in
    {t with content = resolve_content env t.content}

and resolve_content env =
    let open Model.Lang.Compilation_unit in
    function
    | Module m -> Module (resolve_signature env m)
    | Pack _ -> failwith "Unhandled"

and resolve_signature env s =
    let open Model.Lang.Signature in
    let (_, items') = 
        List.fold_right (fun item (env, items) ->
            match item with
            | Module (r, m) ->
                let m' = resolve_module env m in
                let env' = update_env env (`Module m') in
                (env', (Module (r, m'))::items)
            | Type (r, t) ->
                let t' = resolve_type env t in
                let env' = update_env env (`Type t') in
                (env', (Type (r, t'))::items)
            | _ -> failwith "Unhandled") s (env, [])
    in items'

and resolve_module env m =
    let open Model.Lang.Module in
    match m.type_ with
    | ModuleType expr ->
        {m with type_ = ModuleType (resolve_module_type_expr env expr)}
    | _ -> failwith "Unhandled"

and resolve_module_type_expr env expr =
    let open Model.Lang.ModuleType in
    match expr with
    | Signature s -> Signature (resolve_signature env s)
    | _ -> failwith "Unhandled"

and resolve_type env t =
    let open Model.Lang.TypeDecl in
    match t.equation.manifest with
    | Some texpr ->
        let texpr' = resolve_type_expression env texpr in
        {t with equation = {t.equation with manifest = Some texpr'}}
    | None -> t

and resolve_type_expression env texpr =
    let open Model.Lang.TypeExpr in 
    match texpr with
    | Constr (path, ts) ->
        Constr (resolve_path env path, ts)
    | _ -> failwith "Unhandled"

and resolve_path _env p =
    p

and update_env env _m = env
    


let result () =
    let (_,_,s) = myexample in
    resolve_signature [] s
