(** Type of S-expressions *)

type t = Atom of string | List of t list

module Private : sig

  val x : t

end
