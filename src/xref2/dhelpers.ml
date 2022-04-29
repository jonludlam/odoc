(* DHelpers *)

(* Delayed helpers *)

module Module = struct
  let rec canonical :
      Component.Module.t Component.Delayed.t ->
      Odoc_model.Paths.Path.Module.t option = function
    | Val x -> x.Component.Module.canonical
    | OfLang (Module, x, _) -> x.canonical
    | Subst (Module, x, _) -> canonical x

  let rec doc :
      Component.Module.t Component.Delayed.t -> Component.CComment.docs =
    function
    | Val x -> x.doc
    | OfLang (Module, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (Module, x, _) -> doc x

  let rec hidden : Component.Module.t Component.Delayed.t -> bool = function
    | Val x -> x.hidden
    | OfLang (Module, x, _) -> x.hidden
    | Subst (Module, x, _) -> hidden x
end

module ModuleType = struct
  let rec doc :
      Component.ModuleType.t Component.Delayed.t -> Component.CComment.docs =
    function
    | Val x -> x.doc
    | OfLang (ModuleType, x, map) -> Component.Of_Lang.docs map x.doc
    | Subst (ModuleType, x, _) -> doc x

  let rec canonical :
      Component.ModuleType.t Component.Delayed.t ->
      Odoc_model.Paths.Path.ModuleType.t option = function
    | Val x -> x.Component.ModuleType.canonical
    | OfLang (ModuleType, x, _) -> x.canonical
    | Subst (ModuleType, x, _) -> canonical x
end
