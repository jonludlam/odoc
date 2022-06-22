module X : functor ( Y : sig type t end ) -> sig
	module Z = Y
end

(** {!X.Z.t} *)

