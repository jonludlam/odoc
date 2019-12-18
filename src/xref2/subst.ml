type t =
    { module_ : (Ident.module_ * Cpath.resolved_module) list
    ; module_type : (Ident.module_type * Cpath.resolved_module_type) list
    ; type_ : (Ident.path_type * Cpath.resolved_type) list
    ; class_type : (Ident.path_class_type * Cpath.resolved_class_type) list
    }

let identity =
    { module_ = []
    ; module_type = []
    ; type_ = []
    ; class_type = [] }

let add_module id subst t =
    { t with module_ = (id, subst) :: t.module_ }

let add_module_type id subst t =
    { t with module_type = (id, subst) :: t.module_type }

let add_type : Ident.type_ -> Cpath.resolved_type -> t -> t = fun id subst t ->
    { t with type_ = ((id :> Ident.path_type), subst) :: t.type_ }

let add_class : Ident.class_ -> Cpath.resolved_class_type -> t -> t = fun id subst t ->
    { t with type_ = ((id :> Ident.path_type), (subst :> Cpath.resolved_type)) :: t.type_
    ; class_type = ((id :> Ident.path_class_type), subst) :: t.class_type }

let add_class_type : Ident.class_type -> Cpath.resolved_class_type -> t -> t = fun id subst t ->
    { t with type_ = ((id :> Ident.path_type), (subst :> Cpath.resolved_type)) :: t.type_
    ; class_type = ((id :> Ident.path_class_type), subst) :: t.class_type }

let rec resolved_module_path : t -> Cpath.resolved_module -> Cpath.resolved_module = fun s p ->
    match p with
    | `Local id -> begin
        match (try Some (List.assoc id s.module_) with _ -> None) with
        | Some x ->
            x
        | None -> `Local id
        end
    | `Identifier _ ->
        p
    | `Apply (p1, p2) -> `Apply (resolved_module_path s p1, module_path s p2)
    | `Substituted p -> `Substituted (resolved_module_path s p)
    | `Module (p, n) -> `Module (resolved_module_path s p, n)
    | `Alias (p1, p2) -> `Alias (resolved_module_path s p1, resolved_module_path s p2)
    | `Subst (p1, p2) -> `Subst (resolved_module_type_path s p1, resolved_module_path s p2)
    | `SubstAlias (p1, p2) -> `SubstAlias (resolved_module_path s p1, resolved_module_path s p2)
    | `Hidden (p1) -> `Hidden (resolved_module_path s p1)
    | `Canonical (p1, p2) -> `Canonical (resolved_module_path s p1, module_path s p2)

and module_path : t -> Cpath.module_ -> Cpath.module_ = fun s p ->
    match p with
    | `Resolved p' -> `Resolved (resolved_module_path s p')
    | `Dot (p', str) -> `Dot (module_path s p', str)
    | `Apply (p1, p2) -> `Apply (module_path s p1, module_path s p2)
    | `Substituted p -> `Substituted (module_path s p)
    | `Forward _ -> p
    | `Root _ -> p

and resolved_module_type_path : t -> Cpath.resolved_module_type -> Cpath.resolved_module_type = fun s p ->
    match p with
    | `Local id -> begin
        match (try Some (List.assoc id s.module_type) with _ -> None) with
        | Some x ->
            x
        | None -> `Local id
        end
    | `Identifier _ ->
        p
    | `Substituted p -> `Substituted (resolved_module_type_path s p)
    | `ModuleType (p, n) -> `ModuleType (resolved_module_path s p, n)

and module_type_path : t -> Cpath.module_type -> Cpath.module_type = fun s p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_type_path s r)
    | `Substituted p -> `Substituted (module_type_path s p)
    | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_type_path : t -> Cpath.resolved_type -> Cpath.resolved_type = fun s p ->
    match p with
    | `Local id -> begin
        match (try Some (List.assoc id s.type_) with _ -> None) with
        | Some x ->
            x
        | None -> `Local id
        end
    | `Identifier _ ->
        p
    | `Substituted p -> `Substituted (resolved_type_path s p)
    | `Type (p,n) -> `Type (resolved_module_path s p, n)
    | `ClassType (p, n) -> `ClassType (resolved_module_path s p, n)
    | `Class (p, n) -> `Class (resolved_module_path s p, n)

and type_path : t ->  Cpath.type_ -> Cpath.type_ = fun s p ->
    match p with
    | `Resolved r -> `Resolved (resolved_type_path s r)
    | `Substituted p -> `Substituted (type_path s p)
    | `Dot (p, n) -> `Dot (module_path s p, n)

and resolved_class_type_path : t -> Cpath.resolved_class_type -> Cpath.resolved_class_type = fun s p ->
    match p with
    | `Local id -> begin
        match (try Some (List.assoc id s.class_type) with _ -> None) with
        | Some x ->
            x
        | None -> `Local id
        end
    | `Identifier _ ->
        p
    | `Substituted p -> `Substituted (resolved_class_type_path s p)
    | `ClassType (p, n) -> `ClassType (resolved_module_path s p, n)
    | `Class (p, n) -> `Class (resolved_module_path s p, n)

and class_type_path : t -> Cpath.class_type -> Cpath.class_type = fun s p ->
    match p with
    | `Resolved r -> `Resolved (resolved_class_type_path s r)
    | `Substituted p -> `Substituted (class_type_path s p)
    | `Dot (p, n) -> `Dot (module_path s p, n)



let rec module_reference : t -> Cref.module_ -> Cref.module_ = fun t r ->
    match r with
    | `Resolved r -> `Resolved (resolved_module_reference t r)
    | `Root (_,_) -> r
    | `Dot (parent, s) -> `Dot (label_parent_reference t parent, s)
    | `Module (parent, s) -> `Module (signature_reference t parent, s)

and resolved_module_reference : t -> Cref.Resolved.module_ -> Cref.Resolved.module_ = fun t r ->
        match r with
        | `Local id -> begin
            match (try Some (List.assoc id t.module_) with _ -> None) with
            | Some x ->
                let p = Lang_of.(Path.resolved_module empty x) in
                `Identifier (Odoc_model.Paths.Path.Resolved.Module.identifier p)
            | None -> r
            end
        | `Identifier _ -> r
        | `SubstAlias (p1,p2) -> `SubstAlias (resolved_module_path t p1, resolved_module_reference t p2)
        | `Module (p, n) -> `Module (resolved_signature_reference t p, n)
        | `Canonical (m,m2) -> `Canonical (resolved_module_reference t m, module_reference t m2)

and signature_reference : t -> Cref.signature -> Cref.signature = fun t r ->
    match r with
    | `Dot _ | `Module _ as r' -> (module_reference t r' :> Cref.signature)
    | `Root (_,_) -> r
    | `Resolved r -> `Resolved (resolved_signature_reference t r)
    | `ModuleType (parent, s) -> `ModuleType (signature_reference t parent, s)

and resolved_signature_reference : t -> Cref.Resolved.signature -> Cref.Resolved.signature = fun t r ->
match r with
| `Local (#Ident.module_ as id) -> begin
    match (try Some (List.assoc id t.module_) with _ -> None) with
    | Some x ->
        let p = Lang_of.(Path.resolved_module empty x) in
        `Identifier (Odoc_model.Paths.Path.Resolved.Module.identifier p :> Odoc_model.Paths.Identifier.Signature.t)
    | None -> r
    end
| `Local (`LModuleType _ as id) -> begin
    match (try Some (List.assoc id t.module_type) with _ -> None) with
    | Some x ->
        let p = Lang_of.(Path.resolved_module_type empty x) in
        `Identifier (Odoc_model.Paths.Path.Resolved.ModuleType.identifier p :> Odoc_model.Paths.Identifier.Signature.t)
    | None -> r
    end
| `Identifier _ -> r
| `SubstAlias (p1,p2) -> `SubstAlias (resolved_module_path t p1, resolved_module_reference t p2)
| `Module (p, n) -> `Module (resolved_signature_reference t p, n)
| `Canonical (m,m2) -> `Canonical (resolved_module_reference t m, module_reference t m2)
| `ModuleType (p, n) -> `ModuleType (resolved_signature_reference t p, n)


and label_parent_reference : t -> Cref.label_parent -> Cref.label_parent = fun t r ->
match r with
| `Dot _ | `Module _ as r' -> (module_reference t r' :> Cref.label_parent)
| `Root (_,_) -> r
| `Resolved r -> `Resolved (resolved_label_parent_reference t r)
| `ModuleType (parent, s) -> `ModuleType (signature_reference t parent, s)
| `Class (p, n) -> `Class (signature_reference t p, n)
| `ClassType (p, n) -> `ClassType (signature_reference t p, n)
| `Type (p, n) -> `Type (signature_reference t p, n)

and resolved_label_parent_reference : t -> Cref.Resolved.label_parent -> Cref.Resolved.label_parent = fun t r -> 
match r with
| `Local (#Ident.module_ as id) -> begin
    match (try Some (List.assoc id t.module_) with _ -> None) with
    | Some x ->
        let p = Lang_of.(Path.resolved_module empty x) in
        `Identifier (Odoc_model.Paths.Path.Resolved.Module.identifier p :> Odoc_model.Paths.Identifier.LabelParent.t)
    | None -> r
    end
| `Local (`LModuleType _ as id) -> begin
    match (try Some (List.assoc id t.module_type) with _ -> None) with
    | Some x ->
        let p = Lang_of.(Path.resolved_module_type empty x) in
        `Identifier (Odoc_model.Paths.Path.Resolved.ModuleType.identifier p :> Odoc_model.Paths.Identifier.LabelParent.t)
    | None -> r
    end
| `Identifier _ -> r
| `SubstAlias (p1,p2) -> `SubstAlias (resolved_module_path t p1, resolved_module_reference t p2)
| `Module (p, n) -> `Module (resolved_signature_reference t p, n)
| `Canonical (m,m2) -> `Canonical (resolved_module_reference t m, module_reference t m2)
| `ModuleType (p, n) -> `ModuleType (resolved_signature_reference t p, n)
| `Class (p, n) -> `Class (resolved_signature_reference t p, n)
| `ClassType (p, n) -> `ClassType (resolved_signature_reference t p, n)
| `Type (p, n) -> `Type (resolved_signature_reference t p, n)
| `Local _ -> r





let option_ conv s x =
    match x with
    | Some x -> Some (conv s x)
    | None -> None

let list conv s xs =
    List.map (conv s) xs

let rec type_ s t =
    let open Component.TypeDecl in
    let representation = option_ type_decl_representation s t.representation in
    { t with equation = type_decl_equation s t.equation; representation }

and type_decl_representation s t =
    let open Component.TypeDecl.Representation in
    match t with
    | Variant cs -> Variant (List.map (type_decl_constructor s) cs)
    | Record fs -> Record (List.map (type_decl_field s) fs)
    | Extensible -> t

and type_decl_constructor s t =
    let open Component.TypeDecl.Constructor in
    let args = type_decl_constructor_arg s t.args in
    let res = option_ type_expr s t.res in
    {t with args; res }

and type_poly_var s v =
    let open Component.TypeExpr.Polymorphic_variant in
    let map_constr c =
        let open Constructor in
        { name = c.name
        ; constant = c.constant
        ; arguments = List.map (type_expr s) c.arguments
        ; doc = c.doc }
    in
    let map_element = function
    | Type t -> Type (type_expr s t)
    | Constructor c -> Constructor (map_constr c)
    in 
    { kind = v.kind
    ; elements = List.map map_element v.elements}

and type_object s o =
    let open Component.TypeExpr.Object in
    let map_field = function
    | Method m -> Method {m with type_ = type_expr s m.type_}
    | Inherit t -> Inherit (type_expr s t)
    in
    { fields = List.map map_field o.fields
    ; open_ = o.open_ }

and type_package s p =
    let open Component.TypeExpr.Package in
    let sub (x,y) = (x, type_expr s y) in
    { path = module_type_path s p.path
    ; substitutions = List.map sub p.substitutions}

and type_expr s t =
    let open Component.TypeExpr in
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t,str) -> Alias (type_expr s t, str)
    | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr s t1, type_expr s t2)
    | Tuple ts -> Tuple (List.map (type_expr s) ts)
    | Constr (p, ts) -> Constr (type_path s p, List.map (type_expr s) ts)
    | Polymorphic_variant v -> Polymorphic_variant (type_poly_var s v)
    | Object o -> Object (type_object s o)
    | Class (p,ts) -> Class (class_type_path s p, List.map (type_expr s) ts)
    | Poly (strs, ts) -> Poly (strs, type_expr s ts)
    | Package p -> Package (type_package s p)

and module_expansion s t =
    let open Component.Module in
    match t with
    | AlreadyASig -> AlreadyASig
    | Signature sg -> Signature (signature s sg)
    | Functor (arg, sg) -> Functor (List.map (functor_argument_opt s) arg, signature s sg)

and module_type s t =
    let open Component.ModuleType in
    let expr =
        match t.expr with
        | Some m -> Some (module_type_expr s m)
        | None -> None
    in
    let expansion = option_ module_expansion s t.expansion in
    {t with expr; expansion}

and functor_argument_opt s t =
    let open Component.FunctorArgument in
    match t with
    | Some arg ->
        let expansion = option_ module_expansion s arg.expansion in
        Some {arg with expr = module_type_expr s arg.expr; expansion}
    | None ->
        None

and module_type_expr s t =
    let open Component.ModuleType in
    match t with
    | Path p ->
        Path (module_type_path s p)
    | Signature sg ->
        Signature (signature s sg)
    | Functor (arg, expr) ->
        Functor (functor_argument_opt s arg, module_type_expr s expr)
    | With (e,args) ->
        With (module_type_expr s e, List.map (module_type_substitution s) args)
    | TypeOf decl ->
        TypeOf (module_decl s decl)

and module_type_substitution s sub =
    let open Component.ModuleType in
    match sub with
    | ModuleEq (f,m) -> ModuleEq (f, module_decl s m)
    | ModuleSubst (f, p) -> ModuleSubst (f, module_path s p)
    | TypeEq (f, eq) -> TypeEq (f, type_decl_equation s eq)
    | TypeSubst (f, eq) -> TypeSubst (f, type_decl_equation s eq)

and module_decl s t =
    match t with
    | Alias p ->
        Alias (module_path s p)
    | ModuleType t ->
        ModuleType (module_type_expr s t)

and module_ s t =
    let open Component.Module in
    let type_ = module_decl s t.type_ in
    let expansion = option_ module_expansion s t.expansion in
    let canonical = option_ (fun s (m1,m2) -> (module_path s m1, m2)) s t.canonical in
    { t with type_; expansion; canonical }

and module_substitution s m =
    let open Component.ModuleSubstitution in
    let manifest = module_path s m.manifest in
    { m with manifest }

and type_decl_field s f =
    let open Component.TypeDecl.Field in
    { f with type_ = type_expr s f.type_}

and type_decl_constructor_arg s a =
    let open Component.TypeDecl.Constructor in
    match a with
    | Tuple ts -> Tuple (list type_expr s ts)
    | Record fs -> Record (list type_decl_field s fs)

and type_decl_equation s t =
    let open Component.TypeDecl.Equation in
    { t with manifest = option_ type_expr s t.manifest
    ; constraints = List.map (fun (x,y) -> (type_expr s x, type_expr s y)) t.constraints }

and exception_ s e =
    let open Component.Exception in
    let res = option_ type_expr s e.res in
    let args = type_decl_constructor_arg s e.args in
    { e with args; res }

and extension_constructor s c =
    let open Component.Extension.Constructor in
    { c with args = type_decl_constructor_arg s c.args
    ; res = option_ type_expr s c.res }

and extension s e =
    let open Component.Extension in
    { e with
    type_path = type_path s e.type_path;
    constructors = List.map (extension_constructor s) e.constructors
    }

and external_ s e =
    let open Component.External in
    { e with
    type_ = type_expr s e.type_}

and include_ s i =
    let open Component.Include in
    { i with
    decl = module_decl s i.decl;
    expansion_ = apply_sig_map s i.expansion_.items i.expansion_ }

and value s v =
    let open Component.Value in
    { v with
    type_ = type_expr s v.type_ }

and class_ s c =
    let open Component.Class in
    let expansion = option_ class_signature s c.expansion in
    { c with type_ = class_decl s c.type_; expansion }

and class_decl s =
    let open Component.Class in
    function
    | ClassType e -> ClassType (class_type_expr s e)
    | Arrow (lbl, t, d) -> Arrow (lbl, type_expr s t, class_decl s d)

and class_type_expr s =
    let open Component.ClassType in
    function
    | Constr (p, ts) -> Constr (class_type_path s p, List.map (type_expr s) ts)
    | Signature sg -> Signature (class_signature s sg)

and class_type s c =
    let open Component.ClassType in
    let expansion = option_ class_signature s c.expansion in
    { c with expr = class_type_expr s c.expr; expansion }

and class_signature_item s =
    let open Component.ClassSignature in
    function
    | Method (id,m) -> Method (id,method_ s m)
    | InstanceVariable (id,i) -> InstanceVariable (id,instance_variable s i)
    | Constraint (t1, t2) -> Constraint (type_expr s t1, type_expr s t2)
    | Inherit e -> Inherit (class_type_expr s e)
    | Comment _ as y -> y

and class_signature s sg =
    let open Component.ClassSignature in
    { self = option_ type_expr s sg.self
    ; items = List.map (class_signature_item s) sg.items }

and method_ s m =
    let open Component.Method in
    { m with type_ = type_expr s m.type_ }

and instance_variable s i =
    let open Component.InstanceVariable in
    { i with type_ = type_expr s i.type_ }

and rename_bound_idents s sg =
    let open Component.Signature in
    function
    | [] -> s, sg
    | Module (id,r,m) :: rest ->
        let id' = Ident.Rename.module_ id in
        rename_bound_idents
            (add_module id (`Local id') s)
            (Module (id',r,m) :: sg)
            rest
    | ModuleSubstitution (id, m) :: rest ->
        let id' = Ident.Rename.module_ id in
        rename_bound_idents
            (add_module id (`Local id') s)
            (ModuleSubstitution (id',m) :: sg)
            rest
    | ModuleType (id,mt) :: rest ->
        let id' = Ident.Rename.module_type id in
        rename_bound_idents
            (add_module_type id (`Local id') s)
            (ModuleType (id',mt) :: sg)
            rest
    | Type (id,r,t) :: rest ->
        let id' = Ident.Rename.type_ id in
        rename_bound_idents
            (add_type id (`Local (id' :> Ident.path_type)) s)
            (Type (id', r, t) :: sg)
            rest
    | TypeSubstitution (id,t) :: rest ->
        let id' = Ident.Rename.type_ id in
        rename_bound_idents
            (add_type id (`Local (id' :> Ident.path_type)) s)
            (TypeSubstitution (id', t) :: sg)
            rest
    | Exception (id,e) :: rest ->
        let id' = Ident.Rename.exception_ id in
        rename_bound_idents s (Exception (id', e) :: sg) rest
    | TypExt e :: rest ->
        rename_bound_idents s (TypExt e :: sg) rest
    | Value (id,v) :: rest ->
        let id' = Ident.Rename.value id in
        rename_bound_idents s (Value (id',v) :: sg) rest
    | External (id, e) :: rest ->
        let id' = Ident.Rename.value id in
        rename_bound_idents s (External (id',e) :: sg) rest
    | Class (id, r, c) :: rest ->
        let id' = Ident.Rename.class_ id in
        rename_bound_idents
            (add_class id (`Local (id' :> Ident.path_class_type)) s)
            (Class (id', r, c) :: sg)
            rest
    | ClassType (id, r, c) :: rest ->
        let id' = Ident.Rename.class_type id in
        rename_bound_idents
            (add_class_type id (`Local (id' :> Ident.path_class_type)) s)
            (ClassType (id', r, c) :: sg)
            rest
    | Include i :: rest ->
        Format.fprintf Format.err_formatter "rename_bound_idents: %d items\n%!" (List.length i.Component.Include.expansion_.items);
        let (s,items) = rename_bound_idents s [] i.Component.Include.expansion_.items in
        rename_bound_idents s (Include {i with Component.Include.expansion_={items; removed=[]}} :: sg) rest
    | Comment _ as item :: rest ->
        rename_bound_idents s (item :: sg) rest

and removed_items s items =
    let open Component.Signature in
    List.map (function
        | RModule (id, _) when List.mem_assoc id s.module_ ->
            RModule (id, List.assoc id s.module_)
        | x -> x) items

and signature s sg =
    let s, items = rename_bound_idents s [] sg.items in
    apply_sig_map s items sg

and apply_sig_map s items sg =
    let open Component.Signature in
    let items = List.rev_map (function
    | Module (id, r, m) -> Module (id, r, Component.Delayed.put (fun () -> module_ s (Component.Delayed.get m)))
    | ModuleSubstitution (id, m) -> ModuleSubstitution (id, module_substitution s m)
    | ModuleType (id,mt) -> ModuleType (id, module_type s mt)
    | Type (id, r, t) -> Type (id, r, type_ s t)
    | TypeSubstitution (id, t) -> TypeSubstitution (id, type_ s t)
    | Exception (id,e) -> Exception (id, exception_ s e)
    | TypExt e -> TypExt (extension s e)
    | Value (id,v) -> Value (id, value s v)
    | External (id, e) -> External (id, external_ s e)
    | Class (id, r, c) -> Class (id, r, class_ s c)
    | ClassType (id, r, c) -> ClassType (id, r, class_type s c)
    | Include i -> Include (include_ s i)
    | Comment c -> Comment c
    ) items in
    {items; removed = removed_items s sg.removed}
