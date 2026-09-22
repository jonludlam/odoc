open Odoc_unit

type indices_style =
  | Voodoo
  | Normal of { toplevel_content : string option }
  | Automatic

val packages :
  dirs:dirs ->
  prebuilt:Prebuilt.t ->
  remap:bool ->
  indices_style:indices_style ->
  Packages.t list ->
  pkg list
(** The units to build for [pkgs], grouped by package and library, in the order
    the packages were given. With [Normal] indices, the last element is the
    top-level index, which belongs to no package. [prebuilt] describes what an
    earlier run built, for the scopes and search paths to refer to. *)
