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
