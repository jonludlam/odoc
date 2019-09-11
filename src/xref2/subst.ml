type t = {
    map : (Ident.t * Ident.t) list
}

let identity = {
    map = []
}

let add id subst t =
    { map = (id, subst) :: t.map }

let renamed_ids map sg =
    let module_ id map =
        let id' = Ident.rename id in
        add id id' map
    in
    let module_type id map =
        let id' = Ident.rename id in 
        add id id' map
    in
    let type_ id map =
        let id' = Ident.rename id in 
        add id id' map
    in
    let exception_ id map =
        let id' = Ident.rename id in 
        add id id' map
    in
    
    let extension id map =
        let id' = Ident.rename id in 
        add id id' map
        in
    
    let value_ id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let field_ id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let constructor id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let class_ id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let class_type id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let method_ id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let instance_variable id map =
        let id' = Ident.rename id in 
        add id id' map
        in
  
    let fns = {
        Func.Comp.Fold.module_; module_type; type_; exception_; extension; value_; field_; constructor; class_; class_type; method_; instance_variable }
        in
  
    Func.Comp.Fold.fold fns sg.Component.Signature.items map
  
let map_fn_of t =
    let map_fn x = match List.assoc_opt x t.map with Some x -> x | None -> x in
    let open Func.Comp.Map in 
    { module_ = map_fn
    ; module_type = map_fn
    ; type_ = map_fn
    ; exception_ = map_fn
    ; extension = map_fn
    ; value_ = map_fn
    ; field_ = map_fn
    ; constructor = map_fn
    ; class_ = map_fn
    ; class_type = map_fn
    ; method_ = map_fn
    ; instance_variable = map_fn
    }

let signature s sg =
    let new_ids = renamed_ids identity sg in
    let sg' = Func.Comp.Map.map (map_fn_of new_ids) sg in
    Func.Comp.Map.map (map_fn_of s) sg'
