(** Report non-fatal errors *)

(** A failure *)
type t

val catch_failures : (unit -> 'a) -> 'a * t list

(** Report a lookup failure to the enclosing [catch_failures] call. *)
val report : ('fmt, Format.formatter, unit, unit) format4 -> 'fmt

(** Like [report] above but may raise the exception [exn] if strict mode is
    enabled *)
val report_important : exn -> ('fmt, Format.formatter, unit, unit) format4 -> 'fmt

val pp : Format.formatter -> t -> unit

(** [catch_failures f |> shed_failures] will write the failures to stderr and
    return [f ()] *)
val shed_failures : 'a * t list -> 'a
