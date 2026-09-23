(** From what is installed to what to build.

    This module decides, for every package, the units to compile, the identifier
    and the location of each, the [-I] search path of each library and the
    reference scope of the package. All of it is derived from names: what opam
    and findlib say about the switch, and the layout described in {!Odoc_unit}.
*)

(** How the top-level pages are made. *)
type indices_style =
  | Voodoo  (** No top-level page: ocaml.org provides its own. *)
  | Normal of { toplevel_content : string option }
      (** A top-level page listing the packages, or the given [.mld] contents
          instead. *)

val packages :
  dirs:Odoc_unit.dirs ->
  remap:bool ->
  indices_style:indices_style ->
  Packages.t list ->
  Odoc_unit.pkg list
(** The packages to build, in the order given. With [Normal] indices the last
    element is the top-level index, which belongs to no package. With [remap],
    the units of unselected packages are compiled but not rendered, and their
    links are redirected (see {!Packages.t.remaps}). *)
