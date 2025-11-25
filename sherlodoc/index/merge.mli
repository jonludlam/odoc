(** Merge multiple sherlodoc databases into one *)

val merge_databases : Db.Storage.db list -> Db.Storage.db
(** Merge a list of databases by extracting all entries and rebuilding indices.
    Both name search and type search work on merged databases. *)
