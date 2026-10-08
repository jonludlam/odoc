(** The [odoc] command-line tool, as a library.

    The [odoc] executable is just a call to {!main}. A program that registers
    extensions and then calls {!main} is an [odoc] with those extensions
    built in. *)

val main : unit -> unit
(** Run [odoc] on [Sys.argv]. Exits with status 2 on a command-line error. *)
