(** Reading the libraries a [META] file defines.

    Used in voodoo mode, where the package's [META] files are read from the prep
    directory rather than through findlib. *)

type library = {
  name : string;  (** The findlib name, for example [tyxml.functor]. *)
  archive_name : string option;
      (** The archive without its extension, for example [tyxml_f]. [None] for a
          library with no archive: a virtual library, or an alias for other
          libraries. *)
  dir : string option;
      (** The [directory] field: a subdirectory of the [META] file's directory,
          or [None] for the directory itself. *)
  deps : string list;  (** The [requires] field. *)
}

type t = { meta_dir : Fpath.t; libraries : library list }

val process_meta_file : Fpath.t -> t
(** Read a [META] file and the libraries it defines. *)

val dir : t -> library -> Fpath.t
(** The directory holding a library's objects. *)

val libname_of_archive : t -> string Fpath.map
(** Map from the full path of each archive, without extension, to the name of
    its library. *)

val directories : t -> Fpath.set
(** The directories holding the libraries' objects. *)
