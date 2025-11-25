(** CLI for merging multiple sherlodoc databases *)

let merge input_files db_format db_filename =
  (* Load all input databases *)
  let all_shards =
    List.concat_map
      (fun input_file ->
         try
           let module Storage = (val Db_store.storage_module db_format) in
           let shards = Storage.load input_file in
           Format.printf "Loaded %d shard(s) from %s@." (List.length shards) input_file ;
           shards
         with
         | e ->
             Format.fprintf
               Format.err_formatter
               "Warning: Failed to load %s: %s@."
               input_file
               (Printexc.to_string e) ;
             [])
      input_files
  in
  Format.printf "Total shards to merge: %d@." (List.length all_shards) ;
  if all_shards = []
  then begin
    Format.fprintf Format.err_formatter "Error: No databases were successfully loaded@." ;
    exit 1
  end ;
  (* Merge all databases *)
  Format.printf "Merging databases...@." ;
  let merged_db = Index.Merge.merge_databases all_shards in
  (* Save the merged database *)
  Format.printf "Writing merged database to %s...@." db_filename ;
  let module Storage = (val Db_store.storage_module db_format) in
  let h = Storage.open_out db_filename in
  Storage.save ~db:h merged_db ;
  Storage.close_out h ;
  Format.printf "Merge complete!@."

open Cmdliner

let input_files =
  let doc = "Input database file(s) to merge. Must all be in the same format." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"INPUT_DB" ~doc)

let term : (Db_store.db_format -> string -> unit) Term.t =
  Term.(const merge $ input_files)
