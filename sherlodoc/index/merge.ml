(** Merge multiple sherlodoc databases into one *)

module Entry = Db.Entry
module Entry_set = Set.Make (Entry)

(** Extract all entries from a string automaton *)
let rec collect_entries_from_node acc node =
  let open Db.String_automata in
  let acc =
    match node.terminals with
    | Empty -> acc
    | Terminals arr | Summary arr ->
        Array.fold_left (fun acc entry -> Entry_set.add entry acc) acc arr
  in
  match node.children with
  | None -> acc
  | Some children -> Array.fold_left collect_entries_from_node acc children

let collect_entries_from_automaton (automaton : Db.String_automata.t) =
  collect_entries_from_node Entry_set.empty automaton.t

(** Extract all entries from a database *)
let collect_entries_from_db (db : Db.Storage.db) =
  let open Db.Storage in
  (* Collect from name index *)
  let entries = collect_entries_from_automaton db.db_names in
  (* Collect from positive type indices *)
  let entries =
    Occurences.fold
      (fun _count automaton acc ->
         let new_entries = collect_entries_from_automaton automaton in
         Entry_set.union acc new_entries)
      db.db_pos_types
      entries
  in
  (* Collect from negative type indices *)
  let entries =
    Occurences.fold
      (fun _count automaton acc ->
         let new_entries = collect_entries_from_automaton automaton in
         Entry_set.union acc new_entries)
      db.db_neg_types
      entries
  in
  entries

(** Merge multiple databases by extracting entries and rebuilding indices *)
let merge_databases (dbs : Db.Storage.db list) =
  (* Collect all unique entries from all databases *)
  let all_entries =
    List.fold_left
      (fun acc db ->
         let entries = collect_entries_from_db db in
         Entry_set.union acc entries)
      Entry_set.empty
      dbs
  in
  (* Create a new database writer *)
  let db = Db_writer.make () in
  (* Register all entries *)
  Entry_set.iter
    (fun (entry : Entry.t) ->
       (* Register the name *)
       let name = String.lowercase_ascii entry.name in
       Db_writer.store_word db name entry ;
       (* Register type polarities if available *)
       match Entry.Kind.get_type entry.kind with
       | None -> ()
       | Some typ ->
           let polarities = Db.Type_polarity.of_typ ~any_is_poly:true typ in
           Db_writer.store_type_polarities db entry polarities)
    all_entries ;
  (* Export the merged database *)
  Db_writer.export ~summarize:false db
