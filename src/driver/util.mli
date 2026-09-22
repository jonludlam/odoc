(** Small helpers shared by the driver. *)

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

val with_dir :
  Fpath.t option -> Bos.OS.Dir.tmp_name_pat -> (Fpath.t -> unit -> 'a) -> 'a
(** [with_dir dir pattern f] calls [f] with [dir] when it is given. Otherwise it
    creates a temporary directory named after [pattern], calls [f] with it, and
    deletes it afterwards. *)

val lines_of_process : Bos.Cmd.t -> string list
(** Run a command and return the lines it writes to standard output. Fails if
    the command fails. *)

val with_out_to :
  Fpath.t -> (out_channel -> unit) -> (unit, [> `Msg of string ]) result
(** [with_out_to file f] creates the directory of [file] if needed, opens [file]
    for writing and calls [f] with the channel. *)

val cp : string -> string -> unit
(** Copy a file. *)
