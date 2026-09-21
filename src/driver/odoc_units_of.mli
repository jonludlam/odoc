open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  remap:bool ->
  indices_style:indices_style ->
  Packages.t list ->
  pkg list
(** The units to build for [pkgs], grouped by package and library. With [Normal]
    indices, the first element is the top-level index, which belongs to no
    package. *)
