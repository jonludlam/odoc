open Odoc_unit

val make_index :
  dirs:dirs ->
  rel_dir:Fpath.t ->
  obj_dir:Fpath.t ->
  enable_warnings:bool ->
  content:(Format.formatter -> unit) ->
  mld Odoc_unit.t

val library : dirs:dirs -> pkg:Packages.t -> Packages.libty -> mld t

val package : dirs:dirs -> pkg:Packages.t -> mld t

val src : dirs:dirs -> pkg:Packages.t -> mld t

val package_list : dirs:dirs -> remap:bool -> Packages.t list -> mld t

val make_custom : dirs -> Packages.t -> mld t list
