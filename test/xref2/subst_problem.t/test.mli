module type Creators_base = sig
  type 'a t
  type _ concat

  include sig
      type 'a t

      val concat : 'a concat -> 'a t
    end
    with type 'a t := 'a t
end

module type S0_with_creators_base = sig
  type t

  include Creators_base with type _ t := t with type _ concat := t
end

