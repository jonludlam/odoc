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

val prebuilt : Fpath.t -> Odoc_unit.Prebuilt.t
(** [prebuilt odoc_dir] describes the packages and libraries that earlier runs
    of the voodoo driver built into [odoc_dir], found through the marker files
    they wrote with {!write_lib_markers}. *)

val write_lib_markers : Fpath.t -> Packages.t list -> unit
(** [write_lib_markers odoc_dir pkgs] writes marker files to show the locations
    of the compilation units associated with packages and libraries in [pkgs]. A
    library's marker is written in its identifier directory and contains the
    path of the directory holding its [.odoc] files (see
    {!Odoc_unit.lib_obj_dir}). *)
