val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

type pkg

val find_pkg : string -> blessed:bool -> pkg option
(** [get_pkg name ~blessed] looks for a package named [name] in the prep
    directory *)

val of_voodoo : pkg -> Packages.t

val occurrence_file_of_pkg : pkg -> Fpath.t
(** [occurrences_file_of_pkg pkg odoc_dir] returns an appropriate filename for
    the occurrences file for [pkg]. *)

type extra_paths = {
  pkgs : Fpath.t Util.StringMap.t;
  libs : Fpath.t Util.StringMap.t;
  libs_of_pkg : string list Util.StringMap.t;
  lib_deps : Util.StringSet.t Util.StringMap.t;
      (** Each library's declared dependencies, from its {!Lib_marker}. Voodoo
          runs one package at a time and only has that package's [META] files,
          so this is how the library graph is known beyond the current package.
          Libraries whose marker predates {!Lib_marker} are absent. *)
  virtual_cmtis : (string * Fpath.t) list Util.StringMap.t;
      (** Interface digest to the [(module name, stashed [.cmti])] pairs
          offering it. A virtual library's interface is compiled once per
          implementation, so an implementation compiled by a later run — when
          the virtual library's build tree is gone — is compiled against the
          copy stashed here. *)
}
(** What earlier voodoo-mode runs left behind, keyed by package or library name.
    All paths are relative to the [odoc_dir] they were found under. *)

val empty_extra_paths : extra_paths
(** When [odoc_driver] is not running in voodoo mode, this value can be passed
    to {!Odoc_units_of.packages} *)

val extra_paths : Fpath.t -> extra_paths
(** [extra_paths odoc_dir] returns what previous invocations of odoc_driver in
    voodoo mode have compiled below [odoc_dir]. Those invocations must have
    called {!write_lib_markers}. *)

val write_lib_markers : Fpath.t -> Packages.t list -> unit
(** [write_lib_markers odoc_dir pkgs] writes marker files to show the locations
    of the compilation units associated with packages and libraries in [pkgs].
*)
