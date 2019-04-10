module type S = sig
  type 'a t
  val init : string t
end

module type X = sig
    include S
    val doit : 'a t -> unit
end

(* This case doesn't work properly currently. In the end result the type 'a t
 * is substituted for doit but not for mentions from inside S and the
 * definition of the type is still there. It appears that the type is actually
 * substituted away at first but later expand_include comes along and replaces
 * the include expansion with one that hasn't been rewritten. *)
module type X2 = X with type 'a t := 'a list
