(** This module generates json intended to be consumed by search engines. *)

val unit :
  ?occurrences:Odoc_occurrences.Table.t ->
  Format.formatter ->
  simplified:bool ->
  Odoc_model.Lang.Compilation_unit.t ->
  unit
val page : Format.formatter -> simplified:bool -> Odoc_model.Lang.Page.t -> unit
val index :
  ?occurrences:Odoc_occurrences.Table.t ->
  Format.formatter ->
  simplified:bool ->
  Odoc_index.Skeleton.t list ->
  unit

val of_entry :
  ?occurrences:Odoc_occurrences.Table.t ->
  Format.formatter ->
  simplified:bool ->
  Odoc_index.Entry.t ->
  unit
