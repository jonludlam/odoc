open Odoc_model
open Paths
open Names

module rec Resolved : sig
  type parent_unhashed =
    [ `Module of module_ | `ModuleType of module_type | `FragmentRoot ]

  and parent = parent_unhashed Hc.hashed

  and module_unhashed =
    [ `Local of Ident.path_module
    | `Identifier of Identifier.Path.Module.t
    | `Substituted of module_
    | `Subst of module_type * module_
    | `Hidden of module_
    | `Module of parent * ModuleName.t
    | `Canonical of module_ * Path.Module.t
    | `Apply of module_ * module_
    | `Alias of module_ * module_
    | `OpaqueModule of module_ ]

  and module_ = module_unhashed Hc.hashed

  and module_type_unhashed =
    [ `Local of Ident.module_type
    | `Substituted of module_type
    | `Identifier of Identifier.ModuleType.t
    | `ModuleType of parent * ModuleTypeName.t
    | `SubstT of module_type * module_type
    | `AliasModuleType of module_type * module_type
    | `CanonicalModuleType of module_type * Path.ModuleType.t
    | `OpaqueModuleType of module_type ]

  and module_type = module_type_unhashed Hc.hashed

  and type_unhashed =
    [ `Local of Ident.path_type
    | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t
    | `Substituted of type_
    | `CanonicalType of type_ * Path.Type.t
    | `Type of parent * TypeName.t
    | `Class of parent * ClassName.t
    | `ClassType of parent * ClassTypeName.t ]

  and type_ = type_unhashed Hc.hashed

  and class_type_unhashed =
    [ `Local of Ident.path_class_type
    | `Substituted of class_type
    | `Identifier of Odoc_model.Paths.Identifier.Path.ClassType.t
    | `Class of parent * ClassName.t
    | `ClassType of parent * ClassTypeName.t ]

  and class_type = class_type_unhashed Hc.hashed
end =
  Resolved

and Cpath : sig
  type module_unhashed =
    [ `Resolved of Resolved.module_
    | `Substituted of module_
    | `Local of Ident.path_module * bool
    | `Identifier of Identifier.Path.Module.t * bool
    | `Root of string
    | `Forward of string
    | `Dot of module_ * string
    | `Module of Resolved.parent * ModuleName.t (* Like dot, but typed *)
    | `Apply of module_ * module_ ]

  and module_ = module_unhashed Hc.hashed

  and module_type_unhashed =
    [ `Resolved of Resolved.module_type
    | `Substituted of module_type
    | `Local of Ident.module_type * bool
    | `Identifier of Identifier.ModuleType.t * bool
    | `Dot of module_ * string
    | `ModuleType of Resolved.parent * ModuleTypeName.t ]

  and module_type = module_type_unhashed Hc.hashed

  and type_unhashed =
    [ `Resolved of Resolved.type_
    | `Substituted of type_
    | `Local of Ident.path_type * bool
    | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t * bool
    | `Dot of module_ * string
    | `Type of Resolved.parent * TypeName.t
    | `Class of Resolved.parent * ClassName.t
    | `ClassType of Resolved.parent * ClassTypeName.t ]

  and type_ = type_unhashed Hc.hashed

  and class_type_unhashed =
    [ `Resolved of Resolved.class_type
    | `Substituted of class_type
    | `Local of Ident.path_class_type * bool
    | `Identifier of Odoc_model.Paths.Identifier.Path.ClassType.t * bool
    | `Dot of module_ * string
    | `Class of Resolved.parent * ClassName.t
    | `ClassType of Resolved.parent * ClassTypeName.t ]

  and class_type = class_type_unhashed Hc.hashed
end =
  Cpath

include Cpath

module Mk = struct
  module Module = struct
    (* [ `Resolved of Resolved.module_
       | `Substituted of module_
       | `Local of Ident.path_module * bool
       | `Identifier of Identifier.Path.Module.t * bool
       | `Root of string
       | `Forward of string
       | `Dot of module_ * string
       | `Module of Resolved.parent * ModuleName.t (* Like dot, but typed *)
       | `Apply of module_ * module_ ] *)

    let resolved : Resolved.module_ -> module_ = Hc.gen1 (fun x -> `Resolved x)

    let substituted : module_ -> module_ = Hc.gen1 (fun x -> `Substituted x)

    let local : Ident.path_module * bool -> module_ =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Local (x, b)) in
          Hashtbl.add tbl x y;
          y

    let identifier : Identifier.Path.Module.t * bool -> module_ =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Identifier (x, b)) in
          Hashtbl.add tbl x y;
          y

    let root : string -> module_ = Hc.gen_str (fun x -> `Root x)

    let forward : string -> module_ = Hc.gen_str (fun x -> `Forward x)

    let dot : module_ * string -> module_ =
      Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))

    let apply : module_ * module_ -> module_ =
      Hc.gen2 (fun (x, y) -> `Apply (x, y))

    let module_ : Resolved.parent * ModuleName.t -> module_ =
      Hc.gen_named ModuleName.to_string (fun (x, y) -> `Module (x, y))
  end

  module ModuleType = struct
    (* [ `Resolved of Resolved.module_type
       | `Substituted of module_type
       | `Local of Ident.module_type * bool
       | `Identifier of Identifier.ModuleType.t * bool
       | `Dot of module_ * string
       | `ModuleType of Resolved.parent * ModuleTypeName.t ]
    *)

    let resolved : Resolved.module_type -> module_type =
      Hc.gen1 (fun x -> `Resolved x)

    let substituted : module_type -> module_type =
      Hc.gen1 (fun x -> `Substituted x)

    let local : Ident.module_type * bool -> module_type =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Local (x, b)) in
          Hashtbl.add tbl x y;
          y

    let identifier : Identifier.ModuleType.t * bool -> module_type =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Identifier (x, b)) in
          Hashtbl.add tbl x y;
          y

    let dot : module_ * string -> module_type =
      Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))

    let module_type : Resolved.parent * ModuleTypeName.t -> module_type =
      Hc.gen_named ModuleTypeName.to_string (fun (x, y) -> `ModuleType (x, y))
  end

  module Type = struct
    (* [ `Resolved of Resolved.type_
       | `Substituted of type_
       | `Local of Ident.path_type * bool
       | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t * bool
       | `Dot of module_ * string
       | `Type of Resolved.parent * TypeName.t
       | `Class of Resolved.parent * ClassName.t
       | `ClassType of Resolved.parent * ClassTypeName.t ]
    *)

    let resolved : Resolved.type_ -> type_ = Hc.gen1 (fun x -> `Resolved x)

    let substituted : type_ -> type_ = Hc.gen1 (fun x -> `Substituted x)

    let local : Ident.path_type * bool -> type_ =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Local (x, b)) in
          Hashtbl.add tbl x y;
          y

    let identifier : Identifier.Path.Type.t * bool -> type_ =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Identifier (x, b)) in
          Hashtbl.add tbl x y;
          y

    let dot : module_ * string -> type_ =
      Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))

    let type_ : Resolved.parent * TypeName.t -> type_ =
      Hc.gen_named TypeName.to_string (fun (x, y) -> `Type (x, y))

    let class_type : Resolved.parent * ClassTypeName.t -> type_ =
      Hc.gen_named ClassTypeName.to_string (fun (x, y) -> `ClassType (x, y))

    let class_ : Resolved.parent * ClassName.t -> type_ =
      Hc.gen_named ClassName.to_string (fun (x, y) -> `Class (x, y))
  end

  module ClassType = struct
    let resolved : Resolved.class_type -> class_type =
      Hc.gen1 (fun x -> `Resolved x)

    let substituted : class_type -> class_type =
      Hc.gen1 (fun x -> `Substituted x)

    let local : Ident.path_class_type * bool -> class_type =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Local (x, b)) in
          Hashtbl.add tbl x y;
          y

    let identifier : Identifier.Path.ClassType.t * bool -> class_type =
      let tbl = Hashtbl.create 255 in
      fun (x, b) ->
        if Hashtbl.mem tbl x then Hashtbl.find tbl x
        else
          let y = Hc.mk (`Identifier (x, b)) in
          Hashtbl.add tbl x y;
          y

    let dot : module_ * string -> class_type =
      Hc.gen_named (fun x -> x) (fun (x, y) -> `Dot (x, y))

    let class_type : Resolved.parent * ClassTypeName.t -> class_type =
      Hc.gen_named ClassTypeName.to_string (fun (x, y) -> `ClassType (x, y))

    let class_ : Resolved.parent * ClassName.t -> class_type =
      Hc.gen_named ClassName.to_string (fun (x, y) -> `Class (x, y))
  end

  module Resolved = struct
    module Parent = struct
      (*     [ `Module of module_ | `ModuleType of module_type | `FragmentRoot ]
       *)

      let module_ : Resolved.module_ -> Resolved.parent =
        Hc.gen1 (fun x -> `Module x)

      let module_type : Resolved.module_type -> Resolved.parent =
        Hc.gen1 (fun x -> `ModuleType x)

      let fragmentroot : Resolved.parent = Hc.mk `FragmentRoot
    end

    module Module = struct
      (* [ `Local of Ident.path_module
         | `Identifier of Identifier.Path.Module.t
         | `Substituted of module_
         | `Subst of module_type * module_
         | `Hidden of module_
         | `Module of parent * ModuleName.t
         | `Canonical of module_ * Path.Module.t
         | `Apply of module_ * module_
         | `AliasRS of Cpath.module_ * module_
         | `AliasRD of module_ * Cpath.module_
         | `OpaqueModule of module_ ] *)

      let local : Ident.path_module -> Resolved.module_ =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Local x) in
            Hashtbl.add tbl x y;
            y

      let identifier : Identifier.Path.Module.t -> Resolved.module_ =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Identifier x) in
            Hashtbl.add tbl x y;
            y

      let substituted : Resolved.module_ -> Resolved.module_ =
        Hc.gen1 (fun x -> `Substituted x)

      let subst : Resolved.module_type * Resolved.module_ -> Resolved.module_ =
        Hc.gen2 (fun (x, y) -> `Subst (x, y))

      let hidden : Resolved.module_ -> Resolved.module_ =
        Hc.gen1 (fun x -> `Hidden x)

      let module_ : Resolved.parent * Names.ModuleName.t -> Resolved.module_ =
        Hc.gen_named Names.ModuleName.to_string (fun (x, y) -> `Module (x, y))

      let canonical :
          Resolved.module_ * Odoc_model.Paths.Path.Module.t -> Resolved.module_
          =
        Hc.gen2canonical_m (fun (x, y) -> `Canonical (x, y))

      let apply : Resolved.module_ * Resolved.module_ -> Resolved.module_ =
        Hc.gen2 (fun (x, y) -> `Apply (x, y))

      let alias : Resolved.module_ * Resolved.module_ -> Resolved.module_ =
        Hc.gen2 (fun (x, y) -> `Alias (x, y))

      let opaquemodule : Resolved.module_ -> Resolved.module_ =
        Hc.gen1 (fun x -> `OpaqueModule x)
    end

    module ModuleType = struct
      (* [ `Local of Ident.module_type
         | `Substituted of module_type
         | `Identifier of Identifier.ModuleType.t
         | `ModuleType of parent * ModuleTypeName.t
         | `SubstT of module_type * module_type
         | `AliasModuleType of module_type * module_type
         | `CanonicalModuleType of module_type * Path.ModuleType.t
         | `OpaqueModuleType of module_type ]
      *)

      let local : Ident.module_type -> Resolved.module_type =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Local x) in
            Hashtbl.add tbl x y;
            y

      let substituted : Resolved.module_type -> Resolved.module_type =
        Hc.gen1 (fun x -> `Substituted x)

      let identifier : Identifier.Path.ModuleType.t -> Resolved.module_type =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Identifier x) in
            Hashtbl.add tbl x y;
            y

      let module_type :
          Resolved.parent * Names.ModuleTypeName.t -> Resolved.module_type =
        Hc.gen_named Names.ModuleTypeName.to_string (fun (x, y) ->
            `ModuleType (x, y))

      let substt :
          Resolved.module_type * Resolved.module_type -> Resolved.module_type =
        Hc.gen2 (fun (x, y) -> `SubstT (x, y))

      let aliasmoduletype :
          Resolved.module_type * Resolved.module_type -> Resolved.module_type =
        Hc.gen2 (fun (x, y) -> `AliasModuleType (x, y))

      let canonicalmoduletype :
          Resolved.module_type * Odoc_model.Paths.Path.ModuleType.t ->
          Resolved.module_type =
        Hc.gen2canonical_mt (fun (x, y) -> `CanonicalModuleType (x, y))

      let opaquemoduletype : Resolved.module_type -> Resolved.module_type =
        Hc.gen1 (fun x -> `OpaqueModuleType x)
    end

    module Type = struct
      (* [ `Local of Ident.path_type
         | `Identifier of Odoc_model.Paths.Identifier.Path.Type.t
         | `Substituted of type_
         | `CanonicalType of type_ * Path.Type.t
         | `Type of parent * TypeName.t
         | `Class of parent * ClassName.t
         | `ClassType of parent * ClassTypeName.t ]*)

      let local : Ident.path_type -> Resolved.type_ =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Local x) in
            Hashtbl.add tbl x y;
            y

      let substituted : Resolved.type_ -> Resolved.type_ =
        Hc.gen1 (fun x -> `Substituted x)

      let identifier : Identifier.Path.Type.t -> Resolved.type_ =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Identifier x) in
            Hashtbl.add tbl x y;
            y

      let canonicaltype :
          Resolved.type_ * Odoc_model.Paths.Path.Type.t -> Resolved.type_ =
        Hc.gen2canonical_t (fun (x, y) -> `CanonicalType (x, y))

      let type_ : Resolved.parent * Names.TypeName.t -> Resolved.type_ =
        Hc.gen_named Names.TypeName.to_string (fun (x, y) -> `Type (x, y))

      let class_ : Resolved.parent * Names.ClassName.t -> Resolved.type_ =
        Hc.gen_named Names.ClassName.to_string (fun (x, y) -> `Class (x, y))

      let class_type : Resolved.parent * Names.ClassTypeName.t -> Resolved.type_
          =
        Hc.gen_named Names.ClassTypeName.to_string (fun (x, y) ->
            `ClassType (x, y))
    end

    module ClassType = struct
      (* [ `Local of Ident.path_class_type
         | `Substituted of class_type
         | `Identifier of Odoc_model.Paths.Identifier.Path.ClassType.t
         | `Class of parent * ClassName.t
         | `ClassType of parent * ClassTypeName.t ] *)

      let local : Ident.path_class_type -> Resolved.class_type =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Local x) in
            Hashtbl.add tbl x y;
            y

      let substituted : Resolved.class_type -> Resolved.class_type =
        Hc.gen1 (fun x -> `Substituted x)

      let identifier : Identifier.Path.ClassType.t -> Resolved.class_type =
        let tbl = Hashtbl.create 255 in
        fun x ->
          if Hashtbl.mem tbl x then Hashtbl.find tbl x
          else
            let y = Hc.mk (`Identifier x) in
            Hashtbl.add tbl x y;
            y

      let class_ : Resolved.parent * Names.ClassName.t -> Resolved.class_type =
        Hc.gen_named Names.ClassName.to_string (fun (x, y) -> `Class (x, y))

      let class_type :
          Resolved.parent * Names.ClassTypeName.t -> Resolved.class_type =
        Hc.gen_named Names.ClassTypeName.to_string (fun (x, y) ->
            `ClassType (x, y))
    end
  end
end

let rec is_resolved_module_substituted : Resolved.module_ -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Subst (_a, _) -> false (* is_resolved_module_type_substituted a*)
  | `Hidden a | `Canonical (a, _) | `Apply (a, _) | `Alias (a, _) ->
      is_resolved_module_substituted a
  | `Module (a, _) -> is_resolved_parent_substituted a
  | `OpaqueModule a -> is_resolved_module_substituted a

and is_resolved_parent_substituted x =
  match x.v with
  | `Module m -> is_resolved_module_substituted m
  | `ModuleType m -> is_resolved_module_type_substituted m
  | `FragmentRoot -> false

and is_resolved_module_type_substituted : Resolved.module_type -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `ModuleType (a, _) -> is_resolved_parent_substituted a
  | `SubstT _ -> false
  | `AliasModuleType (m1, _) -> is_resolved_module_type_substituted m1
  | `CanonicalModuleType (m, _) | `OpaqueModuleType m ->
      is_resolved_module_type_substituted m

and is_resolved_type_substituted : Resolved.type_ -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `CanonicalType (t, _) -> is_resolved_type_substituted t
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

and is_resolved_class_type_substituted : Resolved.class_type -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Substituted _ -> true
  | `Identifier _ -> false
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_substituted : module_ -> bool =
 fun x ->
  match x.v with
  | `Resolved a -> is_resolved_module_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) | `Apply (a, _) -> is_module_substituted a
  | `Forward _ -> false
  | `Root _ -> false
  | `Module (a, _) -> is_resolved_parent_substituted a

let is_module_type_substituted : module_type -> bool =
 fun x ->
  match x.v with
  | `Resolved a -> is_resolved_module_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `ModuleType (a, _) -> is_resolved_parent_substituted a

let is_type_substituted : type_ -> bool =
 fun x ->
  match x.v with
  | `Resolved a -> is_resolved_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `Type (a, _) | `Class (a, _) | `ClassType (a, _) ->
      is_resolved_parent_substituted a

let is_class_type_substituted : class_type -> bool =
 fun x ->
  match x.v with
  | `Resolved a -> is_resolved_class_type_substituted a
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted _ -> true
  | `Dot (a, _) -> is_module_substituted a
  | `Class (a, _) | `ClassType (a, _) -> is_resolved_parent_substituted a

let rec is_module_forward : module_ -> bool =
 fun x ->
  match x.v with
  | `Forward _ -> true
  | `Resolved _ -> false
  | `Root _ -> false
  | `Identifier _ -> false
  | `Local _ -> false
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_forward p
  | `Module (_, _) -> false

let rec is_module_hidden : module_ -> bool =
 fun x ->
  match x.v with
  | `Resolved r -> is_resolved_module_hidden ~weak_canonical_test:false r
  | `Substituted p | `Dot (p, _) | `Apply (p, _) -> is_module_hidden p
  | `Identifier (_, b) -> b
  | `Local (_, b) -> b
  | `Forward _ -> false
  | `Root _ -> false
  | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_hidden :
    weak_canonical_test:bool -> Resolved.module_ -> bool =
 fun ~weak_canonical_test ->
  let rec inner : Resolved.module_ -> bool =
   fun x ->
    match x.v with
    | `Local _ -> false
    | `Identifier (`Module (_, t)) when ModuleName.is_internal t -> true
    | `Identifier (`Module _) -> false
    | `Identifier _ -> false
    | `Hidden _ -> true
    | `Canonical (_, `Resolved _) -> false
    | `Canonical (p, _) -> (not weak_canonical_test) && inner p
    | `Substituted p -> inner p
    | `Module (p, _) -> is_resolved_parent_hidden ~weak_canonical_test p
    | `Subst (p1, p2) -> is_resolved_module_type_hidden p1 || inner p2
    | `Alias (p1, p2) 
    | `Apply (p1, p2) -> inner p1 || inner p2 
    | `OpaqueModule m -> inner m
  in
  inner

and is_resolved_parent_hidden :
    weak_canonical_test:bool -> Resolved.parent -> bool =
 fun ~weak_canonical_test x ->
  match x.v with
  | `Module m -> is_resolved_module_hidden ~weak_canonical_test m
  | `ModuleType m -> is_resolved_module_type_hidden m
  | `FragmentRoot -> false

and is_module_type_hidden : module_type -> bool =
 fun x ->
  match x.v with
  | `Resolved r -> is_resolved_module_type_hidden r
  | `Identifier (`ModuleType (_, t), b) -> b || ModuleTypeName.is_internal t
  | `Local (_, b) -> b
  | `Substituted p -> is_module_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_module_type_hidden : Resolved.module_type -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Identifier (`ModuleType (_, t)) when ModuleTypeName.is_internal t -> true
  | `Identifier (`ModuleType _) -> false
  | `Substituted p -> is_resolved_module_type_hidden p
  | `ModuleType (p, _) -> is_resolved_parent_hidden ~weak_canonical_test:false p
  | `SubstT (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2
  | `AliasModuleType (p1, p2) ->
      is_resolved_module_type_hidden p1 || is_resolved_module_type_hidden p2
  | `CanonicalModuleType (_, `Resolved _) -> false
  | `CanonicalModuleType (p, _) -> is_resolved_module_type_hidden p
  | `OpaqueModuleType m -> is_resolved_module_type_substituted m

and is_type_hidden : type_ -> bool =
 fun x ->
  match x.v with
  | `Resolved r -> is_resolved_type_hidden r
  | `Identifier (`Type (_, t), b) -> b || TypeName.is_internal t
  | `Identifier (`ClassType (_, t), b) -> b || ClassTypeName.is_internal t
  | `Identifier (`Class (_, t), b) -> b || ClassName.is_internal t
  | `Identifier (`CoreType _, b) -> b
  | `Local (_, b) -> b
  | `Substituted p -> is_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_type_hidden : Resolved.type_ -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Identifier (`Type (_, t)) -> TypeName.is_internal t
  | `Identifier (`ClassType (_, t)) -> ClassTypeName.is_internal t
  | `Identifier (`Class (_, t)) -> ClassName.is_internal t
  | `Identifier (`CoreType _) -> false
  | `Substituted p -> is_resolved_type_hidden p
  | `CanonicalType (_, `Resolved _) -> false
  | `CanonicalType (p, _) -> is_resolved_type_hidden p
  | `Type (p, _) | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_resolved_class_type_hidden : Resolved.class_type -> bool =
 fun x ->
  match x.v with
  | `Local _ -> false
  | `Identifier (`ClassType (_, t)) when ClassTypeName.is_internal t -> true
  | `Identifier (`Class (_, t)) when ClassName.is_internal t -> true
  | `Identifier (`ClassType _) -> false
  | `Identifier (`Class (_, _)) -> false
  | `Substituted p -> is_resolved_class_type_hidden p
  | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

and is_class_type_hidden : class_type -> bool =
 fun x ->
  match x.v with
  | `Resolved r -> is_resolved_class_type_hidden r
  | `Identifier (_, b) -> b
  | `Local (_, b) -> b
  | `Substituted p -> is_class_type_hidden p
  | `Dot (p, _) -> is_module_hidden p
  | `Class (p, _) | `ClassType (p, _) ->
      is_resolved_parent_hidden ~weak_canonical_test:false p

let rec resolved_module_of_resolved_module_reference :
    Reference.Resolved.Module.t -> Resolved.module_ =
  let open Mk.Resolved in
  function
  | `Module (parent, name) ->
      Module.module_
        ( Parent.module_ (resolved_module_of_resolved_signature_reference parent),
          name )
  | `Identifier i -> Module.identifier i
  | `Alias (_m1, _m2) -> failwith "gah"
  | `Hidden s -> Module.hidden (resolved_module_of_resolved_module_reference s)

and resolved_module_of_resolved_signature_reference :
    Reference.Resolved.Signature.t -> Resolved.module_ =
  let open Mk.Resolved in
  function
  | `Identifier (#Identifier.Module.t as i) -> Module.identifier i
  | (`Alias _ | `Module _ | `Hidden _) as r' ->
      resolved_module_of_resolved_module_reference r'
  | `ModuleType (_, n) ->
      failwith ("Not a module reference: " ^ ModuleTypeName.to_string n)
  | `AliasModuleType _ -> failwith "Not a module reference: aliasmoduletype"
  | `Identifier _ -> failwith "Not a module reference : identifier"

and module_of_module_reference : Reference.Module.t -> module_ = function
  | `Resolved r ->
      Mk.Module.resolved (resolved_module_of_resolved_module_reference r)
  | `Root (_, _) -> failwith "unhandled"
  | `Dot
      ( (( `Resolved (`Identifier #Identifier.Module.t)
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      Mk.Module.dot (module_of_module_reference parent, name)
  | `Module
      ( (( `Resolved (`Identifier #Identifier.Module.t)
         | `Dot (_, _)
         | `Module (_, _) ) as parent),
        name ) ->
      Mk.Module.dot
        (module_of_module_reference parent, ModuleName.to_string name)
  | _ -> failwith "Not a module reference"

let rec unresolve_resolved_module_path : Resolved.module_ -> module_ =
  let open Mk.Module in
  fun x ->
    match x.v with
    | `Hidden { v = `Identifier x; _ } -> identifier (x, true)
    | `Identifier x -> identifier (x, false)
    | `Hidden { v = `Local x; _ } -> local (x, true)
    | `Local x -> local (x, false)
    | `Substituted x -> unresolve_resolved_module_path x
    | `Subst (_, x) -> unresolve_resolved_module_path x
    | `Hidden x ->
        unresolve_resolved_module_path x (* should assert false here *)
    | `Module (p, m) ->
        dot (unresolve_resolved_parent_path p, ModuleName.to_string m)
    | `Canonical (m, _) -> unresolve_resolved_module_path m
    | `Apply (m, a) ->
        apply
          (unresolve_resolved_module_path m, unresolve_resolved_module_path a)
    | `Alias (_, m) -> unresolve_resolved_module_path m
    | `OpaqueModule m -> unresolve_resolved_module_path m

and unresolve_module_path : module_ -> module_ =
  let open Mk.Module in
  fun x ->
    match x.v with
    | `Resolved x -> unresolve_resolved_module_path x
    | `Substituted x -> unresolve_module_path x
    | `Local (_, _) -> x
    | `Identifier _ -> x
    | `Root _ -> x
    | `Forward _ -> x
    | `Dot (p, x) -> dot (unresolve_module_path p, x)
    | `Module (p, x) ->
        dot (unresolve_resolved_parent_path p, ModuleName.to_string x)
    | `Apply (x, y) -> apply (unresolve_module_path x, unresolve_module_path y)

and unresolve_resolved_module_type_path : Resolved.module_type -> module_type =
  let open Mk.ModuleType in
  fun x ->
    match x.v with
    | `Local _ | `Identifier _ -> resolved x
    | `Substituted x -> unresolve_resolved_module_type_path x
    | `ModuleType (p, n) ->
        dot (unresolve_resolved_parent_path p, ModuleTypeName.to_string n)
    | `SubstT (_, m) -> unresolve_resolved_module_type_path m
    | `AliasModuleType (_, m2) -> unresolve_resolved_module_type_path m2
    | `CanonicalModuleType (p, _) -> unresolve_resolved_module_type_path p
    | `OpaqueModuleType m -> unresolve_resolved_module_type_path m

and unresolve_resolved_parent_path : Resolved.parent -> module_ =
 fun x ->
  match x.v with
  | `Module m -> unresolve_resolved_module_path m
  | `FragmentRoot | `ModuleType _ -> assert false

and unresolve_resolved_type_path : Resolved.type_ -> type_ =
  let open Mk.Type in
  fun x ->
    match x.v with
    | `Identifier _ | `Local _ -> resolved x
    | `Substituted x -> unresolve_resolved_type_path x
    | `CanonicalType (t1, _) -> unresolve_resolved_type_path t1
    | `Type (p, n) ->
        dot (unresolve_resolved_parent_path p, TypeName.to_string n)
    | `Class (p, n) ->
        dot (unresolve_resolved_parent_path p, ClassName.to_string n)
    | `ClassType (p, n) ->
        dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_resolved_class_type_path : Resolved.class_type -> class_type =
  let open Mk.ClassType in
  fun x ->
    match x.v with
    | `Local _ | `Identifier _ -> resolved x
    | `Substituted x -> unresolve_resolved_class_type_path x
    | `Class (p, n) ->
        dot (unresolve_resolved_parent_path p, ClassName.to_string n)
    | `ClassType (p, n) ->
        dot (unresolve_resolved_parent_path p, ClassTypeName.to_string n)

and unresolve_module_type_path : module_type -> module_type =
 fun x ->
  match x.v with `Resolved m -> unresolve_resolved_module_type_path m | _ -> x

and unresolve_type_path : type_ -> type_ =
 fun x ->
  match x.v with `Resolved m -> unresolve_resolved_type_path m | _ -> x

and unresolve_class_type_path : class_type -> class_type =
 fun x ->
  match x.v with `Resolved m -> unresolve_resolved_class_type_path m | _ -> x
