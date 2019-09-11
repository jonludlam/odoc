open Odoc_model.Paths

module Lang = struct
  open Odoc_model.Lang
  
  type 'a fold_fn =
    { module_ : Identifier.Module.t -> 'a -> 'a
    ; module_type : Identifier.ModuleType.t -> 'a -> 'a
    ; type_ : Identifier.Type.t -> 'a -> 'a
    ; exception_ : Identifier.Exception.t -> 'a -> 'a
    ; extension : Identifier.Extension.t -> 'a -> 'a
    ; value_ : Identifier.Value.t -> 'a -> 'a
    ; field_ : Identifier.Field.t -> 'a -> 'a
    ; constructor : Identifier.Constructor.t -> 'a -> 'a
    ; class_ : Identifier.Class.t -> 'a -> 'a
    ; class_type : Identifier.ClassType.t -> 'a -> 'a
    ; method_ : Identifier.Method.t -> 'a -> 'a
    ; instance_variable : Identifier.InstanceVariable.t -> 'a -> 'a
  }

  let fold fns sg a =
  let rec exception_ e a =
    let open Exception in
    let a = fns.exception_ e.Exception.id a in
    type_decl_constructor_argument e.args a

  and extension e a =
    let open Extension in
    let constructor c a =
        let open Constructor in
        let a = type_decl_constructor_argument c.args a in
        fns.extension c.id a
    in
    List.fold_right constructor e.constructors a

  and type_decl_constructor_argument arg a =
    let open TypeDecl.Constructor in
    match arg with
    | Tuple _ -> a
    | Record fs -> List.fold_right type_decl_field fs a

  and type_decl_constructor c a =
    fns.constructor c.TypeDecl.Constructor.id a

and type_decl_field f a =
    let open TypeDecl.Field in
    fns.field_ f.id a

and type_decl_representation r a =
    let open TypeDecl.Representation in
    match r with
    | Variant cs -> List.fold_right type_decl_constructor cs a
    | Record fs -> List.fold_right type_decl_field fs a
    | Extensible -> a

and functor_argument arg a =
    let open FunctorArgument in
    let a = fns.module_ arg.id a in
    module_type_expr arg.expr a

and module_type_expr e a =
    let open ModuleType in
    match e with
    | Path _
    | Signature _
    | With _
    | TypeOf _ 
    | Functor (None, _) -> a
    | Functor (Some arg, _) ->
      functor_argument arg a
      
and type_decl t a =
    let a =
      match t.TypeDecl.representation with
      | None -> a
      | Some r -> type_decl_representation r a
    in
    fns.type_ t.TypeDecl.id a

and module_ m a =
    fns.module_ m.Module.id a

and module_type m a =
    let open ModuleType in
    let a = fns.module_type m.id a in
    match m.expr with
    | Some e -> module_type_expr e a
    | None -> a

and value_ v a =
    fns.value_ v.Value.id a

and external_ e a =
    fns.value_ e.External.id a

and class_ c a =
    let open Class in
    let a = fns.class_ c.id a in
    class_decl c.type_ a

and class_decl c a =
    let open Class in
    match c with
    | ClassType expr -> class_type_expr expr a
    | Arrow (_, _, decl) -> class_decl decl a

and class_type c a =
    let a = fns.class_type c.ClassType.id a in
    class_type_expr c.ClassType.expr a

and class_type_expr c a =
    let open ClassType in
    match c with
    | Constr _ -> a
    | Signature s -> class_signature s a

and method_ m a =
    fns.method_ m.Method.id a

and instance_variable i a =
    fns.instance_variable i.InstanceVariable.id a

and class_signature sg a =
    let open ClassSignature in
    List.fold_right (fun item a ->
      match item with
      | Method m -> method_ m a
      | InstanceVariable i -> instance_variable i a
      | Constraint _ 
      | Comment _ 
      | Inherit _ -> a
    ) sg.items a

and signature sg a =
    let open Signature in
    List.fold_right (fun item a ->
        match item with
        | Module (_, m) -> module_ m a
        | ModuleType m -> module_type m a
        | Type (_, t) -> type_decl t a
        | Exception e -> exception_ e a          
        | TypExt e -> extension e a
        | Value v -> value_ v a
        | Comment _ -> a
        | External e -> external_ e a
        | Class (_, c) -> class_ c a
        | ClassType (_, c) -> class_type c a
        | Include _i -> a
        ) sg a
  in
  signature sg a
end

module Comp = struct
  open Component
  
  module Fold = struct
  type 'a fold_fn =
    { module_ : Ident.t -> 'a -> 'a
    ; module_type : Ident.t -> 'a -> 'a
    ; type_ : Ident.t -> 'a -> 'a
    ; exception_ : Ident.t -> 'a -> 'a
    ; extension : Ident.t -> 'a -> 'a
    ; value_ : Ident.t -> 'a -> 'a
    ; field_ : Ident.t -> 'a -> 'a
    ; constructor : Ident.t -> 'a -> 'a
    ; class_ : Ident.t -> 'a -> 'a
    ; class_type : Ident.t -> 'a -> 'a
    ; method_ : Ident.t -> 'a -> 'a
    ; instance_variable : Ident.t -> 'a -> 'a
  }

  let fold fns sg a =
    let rec exception_ e a =
      let open Exception in
      let a = fns.exception_ e.Exception.id a in
      type_decl_constructor_argument e.args a
  
    and extension e a =
      let open Extension in
      let constructor c a =
          let open Constructor in
          let a = type_decl_constructor_argument c.args a in
          fns.extension c.id a
      in
      List.fold_right constructor e.constructors a
  
    and type_decl_constructor_argument arg a =
      let open TypeDecl.Constructor in
      match arg with
      | Tuple _ -> a
      | Record fs -> List.fold_right type_decl_field fs a
  
    and type_decl_constructor c a =
      fns.constructor c.TypeDecl.Constructor.id a
  
  and type_decl_field f a =
      let open TypeDecl.Field in
      fns.field_ f.id a
  
  and type_decl_representation r a =
      let open TypeDecl.Representation in
      match r with
      | Variant cs -> List.fold_right type_decl_constructor cs a
      | Record fs -> List.fold_right type_decl_field fs a
      | Extensible -> a
  
  and functor_argument arg a =
      let open FunctorArgument in
      let a = fns.module_ arg.id a in
      module_type_expr arg.expr a
  
  and module_type_expr e a =
      let open ModuleType in
      match e with
      | Path _
      | Signature _
      | With _
      | TypeOf _ 
      | Functor (None, _) -> a
      | Functor (Some arg, _) ->
        functor_argument arg a
        
  and type_decl t a =
      let a =
        match t.TypeDecl.representation with
        | None -> a
        | Some r -> type_decl_representation r a
      in
      fns.type_ t.TypeDecl.id a
  
  and module_ m a =
      fns.module_ m.Module.id a
  
  and module_type m a =
      let open ModuleType in
      let a = fns.module_type m.id a in
      match m.expr with
      | Some e -> module_type_expr e a
      | None -> a
  
  and value_ v a =
      fns.value_ v.Value.id a
  
  and external_ e a =
      fns.value_ e.External.id a
  
  and class_ c a =
      let open Class in
      let a = fns.class_ c.id a in
      class_decl c.type_ a
  
  and class_decl c a =
      let open Class in
      match c with
      | ClassType expr -> class_type_expr expr a
      | Arrow (_, _, decl) -> class_decl decl a
  
  and class_type c a =
      let a = fns.class_type c.ClassType.id a in
      class_type_expr c.ClassType.expr a
  
  and class_type_expr c a =
      let open ClassType in
      match c with
      | Constr _ -> a
      | Signature s -> class_signature s a
  
  and method_ m a =
      fns.method_ m.Method.id a
  
  and instance_variable i a =
      fns.instance_variable i.InstanceVariable.id a
  
  and class_signature sg a =
      let open ClassSignature in
      List.fold_right (fun item a ->
        match item with
        | Method m -> method_ m a
        | InstanceVariable i -> instance_variable i a
        | Constraint _ 
        | Comment _ 
        | Inherit _ -> a
      ) sg.items a
  
  and signature sg a =
      let open Signature in
      List.fold_right (fun item a ->
          match item with
          | Module (_, m) -> module_ m a
          | ModuleType m -> module_type m a
          | Type (_, t) -> type_decl t a
          | Exception e -> exception_ e a          
          | TypExt e -> extension e a
          | Value v -> value_ v a
          | Comment _ -> a
          | External e -> external_ e a
          | Class (_, c) -> class_ c a
          | ClassType (_, c) -> class_type c a
          | Include _ -> a
          ) sg a
    in
  signature sg a
      end

  module Map = struct
  type map_fn =
    { module_ : Ident.t -> Ident.t
    ; module_type : Ident.t -> Ident.t 
    ; type_ : Ident.t -> Ident.t 
    ; exception_ : Ident.t -> Ident.t 
    ; extension : Ident.t -> Ident.t 
    ; value_ : Ident.t -> Ident.t 
    ; field_ : Ident.t -> Ident.t 
    ; constructor : Ident.t -> Ident.t 
    ; class_ : Ident.t -> Ident.t 
    ; class_type : Ident.t -> Ident.t 
    ; method_ : Ident.t -> Ident.t 
    ; instance_variable : Ident.t -> Ident.t 
  }

  exception TypesNeedRefining

  let map fn sg =
    let rec signature_items sg =
      let open Signature in
      List.map (function
        | Module (r, m) -> Module (r, module_ m)
        | ModuleType m -> ModuleType (module_type m)
        | Type (r, t) -> Type (r, type_decl t)
        | Exception e -> Exception (exception_ e)          
        | TypExt e -> TypExt (extension e)
        | Value v -> Value (value_ v)
        | Comment _ as c -> c
        | External e -> External (external_ e)
        | Class (r, c) -> Class (r, class_ c)
        | ClassType (r, c) -> ClassType (r, class_type c)
        | Include i -> Include (include_ i)
        ) sg

    and signature_removed r =
      let open Signature in
      match r with
      | RModule (id, p) ->
        RModule (fn.module_ id, Opt.map resolved_path_module p)
      | RType (id, p) ->
        RModule (fn.type_ id, Opt.map resolved_path_type p)
      
    and signature sg =
      let open Signature in
      let items = signature_items sg.items in
      let removed = List.map signature_removed sg.removed in
      {items; removed}
      
    and path_module : Cpath.t -> Cpath.t = fun p ->
      match p with
      | `Resolved r -> `Resolved (resolved_path_module r)
      | `Root _ -> p
      | `Forward _ -> p
      | `Dot (p,x) -> `Dot (path_module p, x)
      | `Apply (m1, m2) -> `Apply (path_module m1, path_module m2)
      | _ -> raise TypesNeedRefining
    
    and path_module_type : Cpath.t -> Cpath.t = fun p ->
      match p with
      | `Resolved r -> `Resolved (resolved_path_module r)
      | `Dot (m, s) -> `Dot (path_module m, s)
      | _ -> raise TypesNeedRefining
    
    and path_type : Cpath.t -> Cpath.t = fun p ->
      match p with
      | `Resolved r -> `Resolved (resolved_path_type r)
      | `Dot (m, s) -> `Dot (path_module m, s)
      | _ -> raise TypesNeedRefining
    
    and path_class_type : Cpath.t -> Cpath.t = fun p ->
      match p with
      | `Resolved r -> `Resolved (resolved_path_class_type r)
      | `Dot (m, s) -> `Dot (path_module m, s)
      | _ -> raise TypesNeedRefining
    
    and resolved_path_module : Cpath.resolved -> Cpath.resolved = fun p ->
      match p with
      | `Ident x -> `Ident (fn.module_ x)
      | `Substituted r -> `Substituted (resolved_path_module r)
      | `Subst (r1, r2) -> `Subst (resolved_path_module_type r1, resolved_path_module r2)
      | `SubstAlias (r1, r2) -> `SubstAlias (resolved_path_module r1, resolved_path_module r2)
      | `Hidden r -> `Hidden (resolved_path_module r)
      | `Module (p, name) -> `Module (resolved_path_module p, name)
      | `Canonical (p, p1) -> `Canonical (resolved_path_module p, path_module p1)
      | `Apply (p, p1) -> `Apply (resolved_path_module p, path_module p1)
      | `Alias (m1, m2) -> `Alias (resolved_path_module m1, resolved_path_module m2)
      | _ -> raise TypesNeedRefining
    
    and resolved_path_module_type p =
      match p with
      | `Ident x -> `Ident (fn.module_type x)
      | `ModuleType (m, name) -> `ModuleType (resolved_path_module m, name)
      | _ -> raise TypesNeedRefining
    
    and resolved_path_type p =
      match p with
      | `Ident x -> `Ident (fn.type_ x)
      | `Type (m, t) -> `Type (resolved_path_module m, t)
      | `Class (m, t) -> `Class (resolved_path_module m, t)
      | `ClassType (m, t) -> `ClassType (resolved_path_module m, t)
      | _ -> raise TypesNeedRefining
    
    and resolved_path_class_type p =
      match p with
      | `Ident x -> `Ident (fn.class_type x)
      | `Class (m, t) -> `Class (resolved_path_module m, t)
      | `ClassType (m, t) -> `ClassType (resolved_path_module m, t)
      | _ -> raise TypesNeedRefining

    (* TODO - module_types too *) 
    and ident_signature : Ident.t -> Ident.t = fun i ->
      fn.module_ i

    and module_ m =
      let open Module in
      let id = fn.module_ m.id in
      let type_ = module_decl m.type_ in
      let canonical = Opt.map (fun (p, r) -> (path_module p, r)) m.canonical in
      let display_type = Opt.map module_decl m.display_type in
      {m with id; type_; canonical; display_type}

    and module_decl d =
      let open Module in
      match d with
      | Alias p -> Alias (path_module p)
      | ModuleType expr -> ModuleType (module_type_expr expr)
    
    and functor_argument a =
      let open FunctorArgument in
      let id = fn.module_ a.id in
      let expr = module_type_expr a.expr in
      {id; expr}

    and module_type_substitution s =
      let open ModuleType in
      match s with
      | ModuleEq (f, eq) -> ModuleEq (f, module_decl eq)
      | TypeEq (f, eq) -> TypeEq (f, type_decl_equation eq)
      | ModuleSubst (f, p) -> ModuleSubst (f, path_module p)
      | TypeSubst (f, eq) -> TypeEq (f, type_decl_equation eq)
    
    and module_type_expr d =
      let open ModuleType in
      match d with
      | Path p -> Path (path_module_type p)
      | Signature sg -> Signature (signature sg)
      | Functor (Some arg, expr) -> Functor (Some (functor_argument arg), module_type_expr expr)
      | Functor (None, expr) -> Functor (None, module_type_expr expr)
      | With (expr, subs) -> With (module_type_expr expr, List.map module_type_substitution subs)
      | TypeOf d -> TypeOf (module_decl d)
    
    and module_type x =
      let open ModuleType in
      let id = fn.module_type x.id in
      let expr = Opt.map module_type_expr x.expr in
      {x with id; expr}
    
    and include_ i =
      let open Include in
      let parent = ident_signature i.parent in
      let decl = module_decl i.decl in
      {i with parent; decl}

    and type_decl_field f =
      let open TypeDecl.Field in
      let id = fn.field_ f.id in
      let type_ = type_expr f.type_ in
      { f with id; type_ }
    
    and type_decl_constructor_argument arg =
      let open TypeDecl.Constructor in
      match arg with
      | Tuple exprs -> Tuple (List.map type_expr exprs)
      | Record fs -> Record (List.map type_decl_field fs)
    
    and type_decl_constructor c =
      let open TypeDecl.Constructor in
      let id = fn.constructor c.id in
      let args = type_decl_constructor_argument c.args in
      let res = Opt.map type_expr c.res in
      {c with id; args; res}

    and type_decl_representation r =
      let open TypeDecl.Representation in
      match r with
      | Variant cs -> Variant (List.map type_decl_constructor cs)
      | Record fs -> Record (List.map type_decl_field fs)
      | Extensible -> r
  
    and type_decl_equation x =
      let open TypeDecl.Equation in
      let manifest = Opt.map type_expr x.manifest in
      let constraints = List.map (fun (x,y) -> (type_expr x, type_expr y)) x.constraints in 
      {x with manifest; constraints}

    and type_decl x =
      let open TypeDecl in
      let id = fn.type_ x.id in
      let equation = type_decl_equation x.equation in
      let representation = Opt.map type_decl_representation x.representation in
      {x with id; equation; representation}

    and extension_constructor x =
      let open Extension.Constructor in
      let id = fn.extension x.id in
      let args = type_decl_constructor_argument x.args in
      let res = Opt.map type_expr x.res in
      {x with id; args; res}

    and extension x =
      let open Extension in
      let type_path = path_type x.type_path in
      let constructors = List.map extension_constructor x.constructors in
      {x with type_path; constructors}

    and exception_ x =
      let open Exception in
      let id = fn.exception_ x.id in
      let args = type_decl_constructor_argument x.args in
      let res = Opt.map type_expr x.res in
      {x with id; args; res}

    and value_ x =
      let open Value in
      let id = fn.value_ x.id in
      let type_ = type_expr x.type_ in
      {x with id; type_}

    and external_ x =
      let open External in
      let id = fn.value_ x.id in
      let type_ = type_expr x.type_ in
      {x with id; type_}

    and class_decl x =
      let open Class in
      match x with
      | ClassType e -> ClassType (class_type_expr e)
      | Arrow (lbl, e, d) -> Arrow (lbl, type_expr e, class_decl d)

    and class_ x =
      let open Class in
      let id = fn.class_ x.id in
      let type_ = class_decl x.type_ in
      {x with id; type_}

    and class_type_expr x =
      let open ClassType in
      match x with
      | Constr (p, ts) -> Constr (path_class_type p, List.map type_expr ts)
      | Signature sg -> Signature (class_signature sg)
    
    and class_type x =
      let open ClassType in
      let id = fn.class_type x.id in
      let expr = class_type_expr x.expr in
      {x with id; expr}

    and class_signature x =
      let open ClassSignature in
      let item = function
        | Method m -> Method (method_ m)
        | InstanceVariable v -> InstanceVariable (instance_variable v)
        | Constraint (t1, t2) -> Constraint (type_expr t1, type_expr t2)
        | Inherit c -> Inherit (class_type_expr c)
        | Comment _ as c -> c
      in
      let self = Opt.map type_expr x.self in
      let items = List.map item x.items in
      {self; items}
  
    and method_ m =
      let open Method in
      let id = fn.method_ m.id in
      let type_ = type_expr m.type_ in
      {m with id; type_}
    
    and instance_variable i =
      let open InstanceVariable in
      let id = fn.instance_variable i.id in
      let type_ = type_expr i.type_ in
      {i with id; type_}
    
    and type_expr x =
      let open TypeExpr in
      let poly_var v =
        let open Polymorphic_variant in
        let constructor c =
          let open Constructor in
          let arguments = List.map type_expr c.arguments in
          {c with arguments}
        in
        let element e =
          match e with
          | Type t -> Type (type_expr t)
          | Constructor c -> Constructor (constructor c)
        in
        let elements = List.map element v.elements in
        {v with elements}
      in
      let object_ o =
        let open Object in
        let method_ m =
          {m with type_ = type_expr m.type_}
        in
        let field = function
          | Method m -> Method (method_ m)
          | Inherit t -> Inherit (type_expr t)
        in
        let fields = List.map field o.fields in
        {o with fields}
      in
      let package p =
        let open Package in
        let path = path_module_type p.path in
        let substitutions = List.map (fun (x, y) -> (x, type_expr y)) p.substitutions in
        {path; substitutions}
      in
      match x with
      | Var _
      | Any -> x
      | Alias (t, s) -> Alias (type_expr t, s)
      | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr t1, type_expr t2)
      | Tuple ts -> Tuple (List.map type_expr ts)
      | Constr (p, ts) -> Constr (path_type p, List.map type_expr ts)
      | Polymorphic_variant p -> Polymorphic_variant (poly_var p)
      | Object o -> Object (object_ o)
      | Class (p, ts) -> Class (path_class_type p, List.map type_expr ts)
      | Poly (ss, t) -> Poly (ss, type_expr t)
      | Package p -> Package (package p)



    in signature sg
      end
end

