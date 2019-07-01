(* The following types are identical in structure to those in
    {!Lang}, which may well be too complex for our use, we'll
    see as we expand on this *)
module Opt = struct
    let map f = function
      | Some x -> Some (f x)
      | None -> None
end

module rec Module : sig
    type decl =
    | Alias of Cpath.t
    | ModuleType of ModuleType.expr

    type t =
      { id : Ident.t
      ; type_ : decl }
end = Module 

and TypeExpr : sig
    type t =
        | Var of string
        | Constr of Cpath.t * t list 
end = TypeExpr

and FunctorArgument : sig
    type t =
        { id : Ident.t
        ; expr : ModuleType.expr }
end = FunctorArgument

and ModuleType : sig
    type substitution =
        | ModuleEq of Odoc_model.Paths.Fragment.Module.t * Module.decl
        | ModuleSubst of Odoc_model.Paths.Fragment.Module.t * Cpath.t
        | TypeEq of Odoc_model.Paths.Fragment.Type.t * TypeExpr.t option
        | TypeSubst of Odoc_model.Paths.Fragment.Type.t * TypeExpr.t option

    type expr =
        | Path of Cpath.t
        | Signature of Signature.t
        | With of expr * substitution list
        | Functor of FunctorArgument.t option * expr
    type t =
      { id : Ident.t
      ; expr : expr option }
end = ModuleType

and Type : sig
    type t =
      { id : Ident.t
      ; manifest : TypeExpr.t option }
end = Type

and Signature : sig
    type item = 
        | Module of Module.t
        | ModuleType of ModuleType.t
        | Type of Type.t
    
    type t = item list
end = Signature



module Fmt = struct

    let string_of fmt c =
        let b = Buffer.create 100 in
        Format.fprintf (Format.formatter_of_buffer b) "%a%!" fmt c;
        Buffer.contents b

    let rec signature ppf sg =
        let open Signature in
        Format.fprintf ppf "@[<v>";
        List.iter (function
            | Module m ->
                Format.fprintf ppf
                    "@[<v 2>module %a@]@,"
                    module_ m
            | ModuleType mt ->
                Format.fprintf ppf
                    "@[<v 2>module type %a@]@,"
                    module_type mt
            | Type t ->
                Format.fprintf ppf
                    "@[<v 2>type %a@]@," type_ t) sg;
        Format.fprintf ppf "@]"

    and module_decl ppf d =
        let open Module in
        match d with
        | Alias p ->
            Format.fprintf ppf "= %a" path p
        | ModuleType mt ->
            Format.fprintf ppf ": %a" module_type_expr mt

    and module_ ppf m =
        Format.fprintf ppf "%a %a" Ident.fmt m.id module_decl m.type_

    and module_type ppf mt =
        match mt.expr with
        | Some x -> Format.fprintf ppf "%a = %a" Ident.fmt mt.id module_type_expr x
        | None -> Format.fprintf ppf "%a" Ident.fmt mt.id

    and module_type_expr ppf mt =
        let open ModuleType in
        match mt with
        | Path p -> path ppf p
        | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
        | With (expr,subs) -> Format.fprintf ppf "%a with [%a]" module_type_expr expr substitution_list subs
        | Functor (arg, res) -> Format.fprintf ppf "(%a) -> %a" functor_argument_opt arg module_type_expr res

    and functor_argument_opt ppf x =
        match x with
        | None -> ()
        | Some x -> Format.fprintf ppf "%a" functor_argument x
    
    and functor_argument ppf x =
        Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorArgument.id module_type_expr x.FunctorArgument.expr

    and type_decl ppf t =
        match t with
        | Some expr -> Format.fprintf ppf "%a" type_expr expr
        | None -> ()
    
    and type_ ppf t =
        match t.Type.manifest with
        | Some _ -> Format.fprintf ppf "%a = %a" Ident.fmt t.id type_decl t.Type.manifest
        | None -> Format.fprintf ppf "%a" Ident.fmt t.id
 
    and substitution ppf t =
        let open ModuleType in
        match t with
        | ModuleEq (frag, decl) ->
            Format.fprintf ppf "%a = %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_decl decl
        | ModuleSubst (frag, mpath) ->
            Format.fprintf ppf "%a := %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) path mpath
        | TypeEq (frag, decl) ->
            Format.fprintf ppf "%a = %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_decl decl
        | TypeSubst (frag, decl) ->
            Format.fprintf ppf "%a := %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_decl decl

    
    and substitution_list ppf l =
        match l with
        | sub :: (_ :: _) as subs -> Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
        | sub :: [] -> Format.fprintf ppf "%a" substitution sub
        | [] -> ()

    and type_expr ppf e =
        let open TypeExpr in
        match e with
        | Var x -> Format.fprintf ppf "%s" x
        | Constr (p,_args) -> path ppf p

    and path ppf p =
        match p with
        | `Local ident -> Format.fprintf ppf "%a" Ident.fmt ident
        | `Dot (p,str) -> Format.fprintf ppf "%a.%s" path p str
        | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" path p1 path p2
        | `Global p -> Format.fprintf ppf "global(%a)" model_path p
        | `Substituted p -> Format.fprintf ppf "substituted(%a)" path p

    and model_path ppf (p : Odoc_model.Paths.Path.t) =
        match p with
        | `Resolved rp -> model_resolved_path ppf rp
        | `Root s -> Format.fprintf ppf "*%s" s
        | `Forward s -> Format.fprintf ppf "*%s" s
        | `Dot (parent,s) -> Format.fprintf ppf "*%a.%s" model_path (parent :> Odoc_model.Paths.Path.t) s
        | `Apply (func,arg) -> Format.fprintf ppf "*%a(%a)" model_path (func :> Odoc_model.Paths.Path.t) model_path (arg :> Odoc_model.Paths.Path.t)

    and model_resolved_path ppf (p : Odoc_model.Paths.Path.Resolved.t) =
        let open Odoc_model.Paths.Path.Resolved in
        match p with
        | `Identifier id -> Format.fprintf ppf "(%a)" model_identifier id
        | `Module (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (parent,name) -> Format.fprintf ppf "%a.%s" model_resolved_path (parent :> t) (Odoc_model.Names.TypeName.to_string name)
        | `Alias (path, realpath) -> Format.fprintf ppf "(%a -> %a)" model_resolved_path (path :> t) model_resolved_path (realpath :> t)
        | `Subst (modty, m) -> Format.fprintf ppf "(%a subst-> %a)" model_resolved_path (modty :> t) model_resolved_path (m :> t)
        | `Apply (funct, arg) -> Format.fprintf ppf "%a(%a)" model_resolved_path (funct :> t) model_path (arg :> Odoc_model.Paths.Path.t)
        | _ -> Format.fprintf ppf "UNIMPLEMENTED model_resolved_path"
    and model_identifier ppf (p : Odoc_model.Paths.Identifier.t) =
        match p with
        | `Root (_, unit_name) -> Format.fprintf ppf "%s" (Odoc_model.Names.UnitName.to_string unit_name)
        | `Module (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (parent, name) -> Format.fprintf ppf "%a.%s" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.TypeName.to_string name)
        | `Parameter (parent, name) -> Format.fprintf ppf "(param %a %s)" model_identifier (parent :> Odoc_model.Paths.Identifier.t) (Odoc_model.Names.ParameterName.to_string name)
        | `Result parent -> Format.fprintf ppf "%a.result" model_identifier (parent :> Odoc_model.Paths.Identifier.t) 
        | `CoreType name -> Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
        | _ -> Format.fprintf ppf "UNIMPLEMENTED model_identifier"

    and model_fragment ppf (f : Odoc_model.Paths.Fragment.t) =
        match f with
        | `Resolved rf -> model_resolved_fragment ppf rf
        | `Dot (sg, d) -> Format.fprintf ppf "*%a.%s" model_fragment (sg :> Odoc_model.Paths.Fragment.t) d
    
    and model_resolved_fragment ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
        match f with
        | `Root -> ()
        | `Module (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> Odoc_model.Paths.Fragment.Resolved.t) (Odoc_model.Names.ModuleName.to_string m)
        | _ -> Format.fprintf ppf "UNIMPLEMENTED model_resolved_fragment"

end

module Of_Lang = struct
    let ident_of_identifier ident_map identifier =
        List.assoc_opt identifier ident_map

    let rec local_path_of_path : _ -> Odoc_model.Paths.Path.t -> Cpath.t = fun ident_map path ->
        match path with
        | `Resolved (`Identifier i) -> begin
            match ident_of_identifier ident_map i with
            | Some ident -> 
                `Local ident
            | None ->
                `Global path
            end 
        | `Resolved _ ->
            `Global path
        | `Dot (path', x) ->
            `Dot (local_path_of_path ident_map (path' :> Odoc_model.Paths.Path.t), x)
        | `Apply (p1, p2) ->
            `Apply (local_path_of_path ident_map (p1 :> Odoc_model.Paths.Path.t),
                    (local_path_of_path ident_map (p2 :> Odoc_model.Paths.Path.t)))
        | _ -> failwith (Printf.sprintf "local_path_of_path: %s" (Fmt.string_of Fmt.model_path path))

    let rec type_ ident_map id ty =
        let open Odoc_model.Lang.TypeDecl in
        let manifest = type_equation ident_map ty.equation in
        { Type.id; manifest }

    and type_equation ident_map eqn =
        match eqn.Odoc_model.Lang.TypeDecl.Equation.manifest with
        | None -> None
        | Some (Constr (path, _)) ->
            Some (TypeExpr.Constr (local_path_of_path ident_map (path :> Odoc_model.Paths.Path.t), []))
        | Some _ -> failwith "Unhandled in of_type_equation"

    and module_decl ident_map m =
        match m with
        | Odoc_model.Lang.Module.Alias p ->
            Module.Alias (local_path_of_path ident_map (p :> Odoc_model.Paths.Path.t))
        | Odoc_model.Lang.Module.ModuleType s ->
            Module.ModuleType (module_type_expr ident_map s)

    and module_ ident_map id m =
        let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
        {Module.id; type_}

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
            let id = Ident.of_identifier (identifier :> Identifier.t) in
            let ident_map' = ((identifier :> Identifier.t), id) :: ident_map in
            let arg' = functor_argument ident_map' id arg in
            let expr' = module_type_expr ident_map' expr in
            ModuleType.Functor (Some arg', expr')
        | Odoc_model.Lang.ModuleType.Functor (None, expr) ->
            let expr' = module_type_expr ident_map expr in
            ModuleType.Functor (None, expr')
        | _ -> failwith "Unhandled here"
    
    and module_type ident_map id m =
        let expr = Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr in
        {ModuleType.id; expr}

    and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
        let open Odoc_model.Paths in
        fun ident_map items ->
            (* First we construct a list of brand new [Ident.t]s 
                for each item in the signature *)
            let ident_map_new : (Identifier.t * Ident.t) list = List.map (function
                | Odoc_model.Lang.Signature.Type (_, t) ->
                    let identifier = t.Odoc_model.Lang.TypeDecl.id in
                    let id = Ident.of_identifier (identifier :> Identifier.t) in
                    ((identifier :> Identifier.t), id)
                | Odoc_model.Lang.Signature.Module (_, m) ->
                    let identifier = m.Odoc_model.Lang.Module.id in 
                    let id = Ident.of_identifier (identifier :> Identifier.t) in
                    ((identifier :> Identifier.t), id)
                | Odoc_model.Lang.Signature.ModuleType m ->
                    let identifier = m.Odoc_model.Lang.ModuleType.id in
                    let id = Ident.of_identifier (identifier :> Identifier.t) in
                    ((identifier :> Identifier.t), id)
                | _ -> failwith "Unhandled type in of_signature (1)") items
            in

            let ident_map = ident_map_new @ ident_map in
            (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
            List.map2 (fun item (_, id) ->
                match item with
                |  Odoc_model.Lang.Signature.Type (_, t) ->
                    let t' = type_ ident_map id t in
                    Signature.Type t'
                | Odoc_model.Lang.Signature.Module (_, m) ->
                    let m' = module_ ident_map id m in
                    Signature.Module m'
                | Odoc_model.Lang.Signature.ModuleType m ->
                    let m' = module_type ident_map id m in
                    Signature.ModuleType m'
                | _ -> failwith "Unhandled type in of_signature") items ident_map_new

end

module Find = struct

exception Find_failure of Signature.t * string * string

let fail sg name ty = raise (Find_failure (sg, name, ty))

let module_in_sig s name =
    let rec inner = function
      | (Signature.Module m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

let module_type_in_sig s name =
    let rec inner = function
      | (Signature.ModuleType m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

let type_in_sig s name =
    let rec inner = function
      | (Signature.Type m)::_ when (Ident.name m.id)=name -> m
      | _::rest -> inner rest
      | [] -> fail s name "module"
    in inner s

end
