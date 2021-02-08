(**/**)
module For_generated_code : sig
    (*_ don't use this by hand, it is only meant for ppx_fields_conv *)
  
    type ('perm, 'record, 'field) t =
      { force_variance : 'perm -> unit
      ; name : string
      ; setter : ('record -> 'field -> unit) option
      ; getter : 'record -> 'field
      ; fset : 'record -> 'field -> 'record
      }
  
    val opaque_identity : 'a -> 'a
  end
  (**/**)
  
  (**['record] is the type of the record.  ['field] is the type of the
      values stored in the record field with name [name]. ['perm] is a way
      of restricting the operations that can be used. *)
  type ('perm, 'record, 'field) t_with_perm =
    | Field of ('perm, 'record, 'field) For_generated_code.t
  [@@unboxed]
