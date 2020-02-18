(* Simplify *)

exception Clash

let rec type_expr map t =
  let open Odoc_model.Lang.TypeExpr in
  match t with
  | Var v -> List.assoc v map
  | Any -> Any
  | Alias (t, s) -> Alias (type_expr map t, s)
  | Arrow (l, t1, t2) -> Arrow (l, type_expr map t1, type_expr map t2)
  | Tuple ts -> Tuple (List.map (type_expr map) ts)
  | Constr (p, ts) -> Constr (p, List.map (type_expr map) ts)
  | Polymorphic_variant pv -> Polymorphic_variant (polymorphic_variant map pv)
  | Object o -> Object (object_ map o)
  | Class (path, ts) -> Class (path, List.map (type_expr map) ts)
  | Poly (s, t) -> Poly (s, type_expr map t)
  | Package p -> Package (package map p)

and polymorphic_variant map pv =
  let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
  let constructor c =
    { c with Constructor.arguments = List.map (type_expr map) c.Constructor.arguments }
  in
  let element = function
    | Type t -> Type (type_expr map t)
    | Constructor c -> Constructor (constructor c)
  in
  { kind = pv.kind
  ; elements = List.map element pv.elements }

and object_ map o =
  let open Odoc_model.Lang.TypeExpr.Object in
  let method_ m =
    { m with type_ = type_expr map m.type_ }
  in
  let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expr map t)
  in
  { o with fields = List.map field o.fields }

and package map p =
  let open Odoc_model.Lang.TypeExpr.Package in
  let subst (frag, t) = (frag, type_expr map t) in
  {p with substitutions = List.map subst p.substitutions}

let collapse_eqns eqn1 eqn2 params =
  let open Odoc_model.Lang.TypeDecl in
  let map = List.map2 (fun v p ->
    match v with
    | (Var x, _) -> (x, p)
    | _ -> failwith "Unexpected type parameter") eqn2.Equation.params params in
  {eqn1 with Equation.manifest = match eqn2.manifest with | None -> None | Some t -> Some (type_expr map t) }