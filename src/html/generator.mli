val functor_arg_pos : Odoc_model.Lang.FunctorArgument.t -> int
val keyword : string -> [> Html_types.span ] Tyxml.Html.elt

include module type of Generator_signatures
module Make (Syntax : SYNTAX) : GENERATOR
