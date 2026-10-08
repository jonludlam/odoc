(** The custom tags that comments are parsed with.

    A custom tag [@name] or [@name.variant] belongs to the prefix [name].
    Registering the prefix makes odoc parse those tags as custom tags, rather
    than as unknown tags. Extensions register their prefixes with
    [Odoc_document.Extension.register_tag]. *)

val register : string -> unit
(** [register prefix] makes [@prefix] and [@prefix.variant] custom tags. *)

val prefix : string -> string
(** [prefix name] is the part of [name] before the first ['.']. *)

val is_custom : string -> bool
(** [is_custom name] is [true] if the prefix of [name] is registered. *)

val is_valid_prefix : string -> bool
(** [is_valid_prefix p] is [true] if the tags [@p] and [@p.variant] are parsed
    as custom tags once [p] is registered: [p] is a name, like [admonition] or
    [x-ocaml], whose letters are not the name of one of odoc's own tags, like
    [deprecated] or [see-also]. *)
