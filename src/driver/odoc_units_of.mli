open Odoc_unit

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  pkg_list:bool ->
  Packages.t list ->
  t list
