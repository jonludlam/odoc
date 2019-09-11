(* The following types are identical in structure to those in
    {!Lang}, which may well be too complex for our use, we'll
    see as we expand on this *)

module Comment = Odoc_model.Comment

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
      ; doc : Comment.docs
      ; type_ : decl 
      ; canonical : (Cpath.t * Odoc_model.Paths.Reference.Module.t) option
      ; hidden : bool
      ; display_type : decl option
      }
end = Module


and FunctorArgument : sig
    type t =
        { id : Ident.t
        ; expr : ModuleType.expr }
end = FunctorArgument

and ModuleType : sig
    type substitution =
        | ModuleEq of Odoc_model.Paths.Fragment.Module.t * Module.decl
        | ModuleSubst of Odoc_model.Paths.Fragment.Module.t * Cpath.t
        | TypeEq of Odoc_model.Paths.Fragment.Type.t * TypeDecl.Equation.t
        | TypeSubst of Odoc_model.Paths.Fragment.Type.t * TypeDecl.Equation.t

    type expr =
        | Path of Cpath.t
        | Signature of Signature.t
        | With of expr * substitution list
        | Functor of FunctorArgument.t option * expr
        | TypeOf of Module.decl
    type t =
      { id : Ident.t
      ; doc : Comment.docs
      ; expr : expr option }
end = ModuleType

and Signature : sig
    type recursive = Odoc_model.Lang.Signature.recursive

    type item =
        | Module of recursive * Module.t
        | ModuleType of ModuleType.t
        | Type of recursive * TypeDecl.t
        | TypExt of Extension.t
        | Exception of Exception.t
        | Value of Value.t
        | External of External.t
        | Class of recursive * Class.t
        | ClassType of recursive * ClassType.t
        | Include of Include.t
        | Comment of Comment.docs_or_stop

    (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
    type removed_item =
        | RModule of Ident.t * Cpath.resolved option
        | RType of Ident.t * Cpath.resolved option

    type t =
        { items: item list
        ; removed: removed_item list }
end = Signature

and Include : sig
    type t =
        { parent: Ident.t
        ; doc: Comment.docs
        ; decl: Module.decl }
end = Include


and TypeDecl : sig

    module Field : sig
        type t =
            { id : Ident.t
            ; doc : Comment.docs
            ; mutable_ : bool
            ; type_ : TypeExpr.t }
    end

    module Constructor : sig
        type argument =
            | Tuple of TypeExpr.t list
            | Record of Field.t list
        
        type t = 
            { id : Ident.t
            ; doc : Comment.docs
            ; args : argument
            ; res: TypeExpr.t option }
    end

    module Representation : sig
        type t =
            | Variant of Constructor.t list
            | Record of Field.t list
            | Extensible
    end

    type param = Odoc_model.Lang.TypeDecl.param

    module Equation : sig
        type t =
            { params: param list
            ; private_ : bool
            ; manifest : TypeExpr.t option
            ; constraints : (TypeExpr.t * TypeExpr.t) list}
    end

    type t =
      { id : Ident.t
      ; doc : Comment.docs
      ; equation : Equation.t
      ; representation : Representation.t option }
end = TypeDecl

and Extension : sig
    module Constructor : sig

        type t =
          { id: Ident.t;
            doc: Comment.docs;
            args: TypeDecl.Constructor.argument;
            res: TypeExpr.t option; }
    
      end
    
      type t =
        { type_path: Cpath.t;
          doc: Comment.docs;
          type_params: TypeDecl.param list;
          private_: bool;
          constructors: Constructor.t list; }
    
end = Extension
and Exception : sig

    type t = 
        { id: Ident.t
        ; doc : Comment.docs
        ; args : TypeDecl.Constructor.argument
        ; res : TypeExpr.t option }

end = Exception


and Value : sig

    type t =
      { id: Ident.t;
        doc: Comment.docs;
        type_: TypeExpr.t; }
  
end = Value

and External : sig
    type t =
        { id : Ident.t
        ; doc : Comment.docs
        ; type_ : TypeExpr.t
        ; primitives : string list }

end = External

and Class : sig
    type decl =
        | ClassType of ClassType.expr
        | Arrow of TypeExpr.label option * TypeExpr.t * decl
    
    type t =
        { id : Ident.t
        ; doc : Comment.docs
        ; virtual_ : bool
        ; params: TypeDecl.param list
        ; type_ : decl }
end = Class

and ClassType : sig
    type expr =
        | Constr of Cpath.t * TypeExpr.t list
        | Signature of ClassSignature.t
    
    type t = 
        { id : Ident.t 
        ; doc : Comment.docs
        ; virtual_ : bool
        ; params: TypeDecl.param list
        ; expr: expr }
end = ClassType

and ClassSignature : sig
    type item =
        | Method of Method.t
        | InstanceVariable of InstanceVariable.t
        | Constraint of TypeExpr.t * TypeExpr.t
        | Inherit of ClassType.expr
        | Comment of Comment.docs_or_stop
    
    type t =
        { self: TypeExpr.t option
        ; items: item list }
end = ClassSignature

and Method : sig
    type t =
        { id : Ident.t
        ; doc : Comment.docs
        ; private_: bool
        ; virtual_: bool
        ; type_ : TypeExpr.t }
end = Method

and InstanceVariable : sig
    type t =
        { id: Ident.t
        ; doc: Comment.docs
        ; mutable_ : bool
        ; virtual_ : bool
        ; type_ : TypeExpr.t }
end = InstanceVariable
and TypeExpr : sig

    module Polymorphic_variant : sig
        type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind
    
        module Constructor :
        sig
          type t = {
            name : string;
            constant : bool;
            arguments : TypeExpr.t list;
            doc : Comment.docs;
          }
        end
    
        type element =
          | Type of TypeExpr.t
          | Constructor of Constructor.t
    
        type t =
          { kind: kind;
            elements: element list;}
    end

    module Object : sig

        type method_ =
          { name: string;
            type_: TypeExpr.t; }
    
        type field =
          | Method of method_
          | Inherit of TypeExpr.t
    
        type t =
          { fields: field list;
            open_ : bool; }
    
      end
    
      module Package : sig
    
        type substitution = Odoc_model.Paths.Fragment.Type.t * TypeExpr.t
    
        type t =
          { path: Cpath.t;
            substitutions: substitution list; }
    
      end
    
    type label = Odoc_model.Lang.TypeExpr.label
    type t =
        | Var of string
        | Any
        | Alias of t * string
        | Arrow of label option * t * t
        | Tuple of t list
        | Constr of Cpath.t * t list
        | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
        | Object of TypeExpr.Object.t
        | Class of Cpath.t * t list
        | Poly of string list * t
        | Package of TypeExpr.Package.t

end = TypeExpr


  

module Element = struct
    open Odoc_model.Paths
    type module_ = [ `Module of Identifier.Module.t * Module.t ]
    type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]
    type type_ = [ `Type of Identifier.Type.t * TypeDecl.t ]
    type value = [ `Value of Identifier.Value.t * Value.t ]
    type label = [ `Label of Identifier.Label.t ]
    type signature = [ module_ | module_type ]
    type any = [ signature | value | type_ | label ]
end



module Fmt = struct
    let option formatter fmt x =
        match x with
        | Some x' -> Format.fprintf fmt "%a" formatter x'
        | None -> ()

    let string_of fmt c =
        let b = Buffer.create 100 in
        Format.fprintf (Format.formatter_of_buffer b) "%a%!" fmt c;
        Buffer.contents b

    let rec signature ppf sg =
        let open Signature in
        Format.fprintf ppf "@[<v>";
        List.iter (function
            | Module (_,m) ->
                Format.fprintf ppf
                    "@[<v 2>module %a@]@,"
                    module_ m
            | ModuleType mt ->
                Format.fprintf ppf
                    "@[<v 2>module type %a@]@,"
                    module_type mt
            | Type (_,t) ->
                Format.fprintf ppf
                    "@[<v 2>type %a@]@," type_decl t
            | TypExt e ->
                Format.fprintf ppf
                    "@[<v 2>type_extension %a@]@," extension e
            | Exception e ->
                Format.fprintf ppf
                    "@[<v 2>exception %a@]@," exception_ e
            | Value v ->
                Format.fprintf ppf
                    "@[<v 2>val %a@]@," value v
            | External e ->
                Format.fprintf ppf
                    "@[<v 2>external %a@]@," external_ e
            | Class (_,c) ->
                Format.fprintf ppf
                    "@[<v 2>class %a@]@," class_ c
            | ClassType (_,c) ->
                Format.fprintf ppf
                    "@[<v 2>class type %a@]@," class_type c
            | Include i ->
                Format.fprintf ppf
                    "@[<v 2>include %a@]@," include_ i
            | Comment _c ->
                ()
            ) sg.items;
        Format.fprintf ppf "@]"

    and external_ ppf _ =
        Format.fprintf ppf "<todo>"
    
    and class_ ppf _c =
        Format.fprintf ppf "<todo>"
    
    and class_type ppf _c =
        Format.fprintf ppf "<todo>"
    
    and include_ ppf _i =
        Format.fprintf ppf "<todo>"

        and value ppf v =
        let open Value in
        Format.fprintf ppf "%a : %a" Ident.fmt v.id type_expr v.type_

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
        | TypeOf decl -> Format.fprintf ppf "module type of %a" module_decl decl

    and functor_argument_opt ppf x =
        match x with
        | None -> ()
        | Some x -> Format.fprintf ppf "%a" functor_argument x

    and functor_argument ppf x =
        Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorArgument.id module_type_expr x.FunctorArgument.expr

    and type_decl ppf t =
        match TypeDecl.(t.equation.Equation.manifest) with
        | Some x -> Format.fprintf ppf "%a = %a" Ident.fmt t.id type_expr x
        | None -> Format.fprintf ppf "%a" Ident.fmt t.id

    and type_equation ppf t =
        match t.TypeDecl.Equation.manifest with
        | None -> ()
        | Some m -> Format.fprintf ppf " = %a" type_expr m

    and type_equation2 ppf t =
        match t.TypeDecl.Equation.manifest with
        | None -> ()
        | Some m -> Format.fprintf ppf " = %a" type_expr m

    and exception_ ppf e =
        Format.fprintf ppf "%a" Ident.fmt e.Exception.id
    
    and extension ppf e =
        Format.fprintf ppf "%a" path e.Extension.type_path

    and substitution ppf t =
        let open ModuleType in
        match t with
        | ModuleEq (frag, decl) ->
            Format.fprintf ppf "%a = %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) module_decl decl
        | ModuleSubst (frag, mpath) ->
            Format.fprintf ppf "%a := %a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) path mpath
        | TypeEq (frag, decl) ->
            Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation decl
        | TypeSubst (frag, decl) ->
            Format.fprintf ppf "%a%a" model_fragment (frag :> Odoc_model.Paths.Fragment.t) type_equation2 decl


    and substitution_list ppf l =
        match l with
        | sub :: (_ :: _) as subs -> Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
        | sub :: [] -> Format.fprintf ppf "%a" substitution sub
        | [] -> ()

    and type_expr_list ppf l =
        match l with
        | t :: (_ :: _) as ts -> Format.fprintf ppf "%a * %a" type_expr t type_expr_list ts
        | t :: [] -> Format.fprintf ppf "%a" type_expr t
        | [] -> ()

    and type_object ppf _o =
        Format.fprintf ppf "(object)"
    
    and type_class ppf (_x,_y) =
        Format.fprintf ppf "(class)"
    
    and type_package ppf _p =
        Format.fprintf ppf "(package)"

    and type_expr ppf e =
        let open TypeExpr in
        match e with
        | Var x -> Format.fprintf ppf "%s" x
        | Any -> Format.fprintf ppf "_"
        | Alias (x,y) -> Format.fprintf ppf "(alias %a %s)" type_expr x y
        | Arrow (_l, t1, t2) -> Format.fprintf ppf "%a -> %a" type_expr t1 type_expr t2
        | Tuple ts -> Format.fprintf ppf "(%a)" type_expr_list ts
        | Constr (p,_args) -> path ppf p
        | Polymorphic_variant _poly -> Format.fprintf ppf "(poly_var)"
        | Object x -> type_object ppf x
        | Class (x,y) -> type_class ppf (x,y)
        | Poly (_ss,_t) -> Format.fprintf ppf "(poly)" 
        | Package x -> type_package ppf x
    
    and resolved_path : Format.formatter -> Cpath.resolved -> unit = fun ppf p ->
        match p with
        | `Ident ident -> Format.fprintf ppf "%a" Ident.fmt ident
        | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" resolved_path p1 path p2
        | `Substituted p -> Format.fprintf ppf "substituted(%a)" resolved_path p
        | `Module (p, m) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ModuleName.to_string m)
        | `ModuleType (p, mt) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ModuleTypeName.to_string mt)
        | `Type (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.TypeName.to_string t)
        | `Alias (p1, p2) -> Format.fprintf ppf "(alias %a -> %a)" resolved_path p1 resolved_path p2
        | `Subst (p1, p2) -> Format.fprintf ppf "(subst %a -> %a)" resolved_path p1 resolved_path p2
        | `SubstAlias (p1, p2) -> Format.fprintf ppf "(substalias %a -> %a)" resolved_path p1 resolved_path p2
        | `Hidden p1 -> Format.fprintf ppf "(hidden %a)" resolved_path p1
        | `Canonical (p1, p2) -> Format.fprintf ppf "(canonical %a -> %a)" resolved_path p1 path p2
        | `Class (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ClassName.to_string t)
        | `ClassType (p, t) -> Format.fprintf ppf "%a.%s" resolved_path p (Odoc_model.Names.ClassTypeName.to_string t)
        

    and path ppf p =
        match p with
        | `Resolved p -> Format.fprintf ppf "%a" resolved_path p
        | `Dot (p,str) -> Format.fprintf ppf "%a.%s" path p str
        | `Apply (p1, p2) -> Format.fprintf ppf "%a(%a)" path p1 path p2
        | `Substituted p -> Format.fprintf ppf "substituted(%a)" path p
        | `Forward s -> Format.fprintf ppf "forward(%s)" s
        | `Root r -> Format.fprintf ppf "%s" r

    and model_path : Format.formatter -> Odoc_model.Paths.Path.t -> unit =
        fun ppf (p : Odoc_model.Paths.Path.t) ->
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
        | `Module (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.ModuleName.to_string name)
        | `ModuleType (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.ModuleTypeName.to_string name)
        | `Type (`Root _, name) -> Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
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
        let open Odoc_model.Paths.Fragment.Resolved in
        match f with
        | `Root -> ()
        | `Module (sg, m) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ModuleName.to_string m)
        | `Type (sg, t) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.TypeName.to_string t)
        | `Subst (path, m) -> Format.fprintf ppf "(%a subst -> %a)" model_resolved_path (path :> Odoc_model.Paths.Path.Resolved.t) model_resolved_fragment (m :> t)
        | `SubstAlias (_,_) -> Format.fprintf ppf "UNIMPLEMENTED subst alias!?"
        | `Class (sg, c) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ClassName.to_string c)
        | `ClassType (sg, c) -> Format.fprintf ppf "%a.%s" model_resolved_fragment (sg :> t) (Odoc_model.Names.ClassTypeName.to_string c)
end



module Find = struct

exception Find_failure of Signature.t * string * string

let fail sg name ty = raise (Find_failure (sg, name, ty))

type 'a found =
    | Found of 'a
    | Replaced of Cpath.resolved

let careful_module_in_sig s name =
    let rec inner_removed = function
      | (Signature.RModule (id, Some p))::_ when (Ident.name id) = name -> Replaced p
      | _::rest -> inner_removed rest
      | [] -> fail s name "module" in
    let rec inner = function
      | (Signature.Module (_,m))::_ when (Ident.name m.id)=name -> Found m
      | _::rest -> inner rest
      | [] -> inner_removed s.removed
    in inner s.items

let careful_type_in_sig s name =
    let rec inner_removed = function
      | (Signature.RType (id, Some p))::_ when (Ident.name id) = name -> Replaced p
      | _::rest -> inner_removed rest
      | [] -> fail s name "type" in
    let rec inner = function
      | (Signature.Type (_,m))::_ when (Ident.name m.id)=name -> Found m
      | _::rest -> inner rest
      | [] -> inner_removed s.removed
    in inner s.items

let module_in_sig s name =
    match careful_module_in_sig s name with
    | Found m -> m
    | Replaced _ -> fail s name "module"

let module_type_in_sig s name =
    let rec inner = function
        | (Signature.ModuleType m)::_ when (Ident.name m.id)=name -> m
        | _::rest -> inner rest
        | [] -> fail s name "module type"
    in inner s.items

let type_in_sig s name =
    match careful_type_in_sig s name with
    | Found t -> t
    | Replaced _ -> fail s name "type"
end
