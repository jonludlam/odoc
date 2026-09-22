(** Running commands and keeping their output for the final report.

    The driver prints the warnings that odoc emitted only at the end of a run,
    grouped by command. This module submits commands to the {!Worker_pool} and
    keeps the output of those the caller asks to log. *)

type log_dest =
  [ `Compile
  | `Compile_src
  | `Link
  | `Count_occurrences
  | `Generate
  | `Index
  | `Sherlodoc
  | `Classify ]
(** The step a logged command belongs to. *)

type log_line = {
  log_dest : log_dest;
  prefix : string;
      (** Identifies the command in the report, usually its input file. *)
  run : Run.t;
}

val outputs : log_line list ref
(** The logged commands, in the order they finished. *)

val submit :
  (log_dest * string) option ->
  string ->
  Bos.Cmd.t ->
  Fpath.t option ->
  string list
(** [submit log description cmd output_file] runs [cmd] on a worker and returns
    the lines it wrote to standard output. When [log] is [Some (dest, prefix)],
    the run is added to {!outputs}. Raises {!Worker_pool.Worker_failure} if the
    command fails. *)

val submit_ignore_failures :
  (log_dest * string) option -> string -> Bos.Cmd.t -> Fpath.t option -> unit
(** Like {!submit}, but a failing command is reported as an error and otherwise
    ignored. *)
