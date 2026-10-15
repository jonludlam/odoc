(** The [odoc] command-line tool, as a library.

    The [odoc] executable is just a call to {!main}. A program that calls
    {!main} with extensions is an [odoc] with those extensions built in. *)

val main : ?extensions:Odoc_extension.t list -> unit -> unit
(** Run [odoc] on [Sys.argv], with [extensions] (by default none). Exits with
    status 2 on a command-line error, or if two extensions handle the same
    thing. *)
