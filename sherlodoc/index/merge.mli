(** Merge multiple sherlodoc databases into one *)

(** Merge a list of databases by extracting all entries and rebuilding indices.
    Note: Type search indices are not preserved during merge; only name search
    will work on the merged database. *)
val merge_databases : Db.Storage.db list -> Db.Storage.db
