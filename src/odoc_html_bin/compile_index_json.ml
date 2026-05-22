open Odoc_utils
open ResultMonad
module Fs = Odoc_odoc.Fs

let compile_to_json ~output ~occurrences ~wrap ~simplified hierarchies =
  Fs.Directory.mkdir_p (Fs.File.dirname output);
  Io_utils.with_formatter_out (Fs.File.to_string output) @@ fun fmt ->
  if wrap then Format.fprintf fmt "let documents = ";
  let all =
    List.fold_left
      (fun acc hierarchy ->
        Tree.fold_left
          ~f:(fun acc entry ->
            Odoc_json_index.Json_search.of_entry ~simplified ?occurrences entry
            :: acc)
          acc hierarchy)
      [] hierarchies
  in
  Format.fprintf fmt "%s" (Json.to_string (`Array (List.rev all)));
  if wrap then
    Format.fprintf fmt
      ";\n\
       const options = { keys: ['name', 'comment'] };\n\
       var idx_fuse = new Fuse(documents, options);\n";
  Ok ()

let compile ~output ~warnings_options ~occurrences ~roots ~inputs_in_file
    ~odocls ~simplified ~wrap =
  Odoc_odoc.Indexing.build_hierarchies ~warnings_options ~occurrences ~roots
    ~inputs_in_file ~odocls
  >>= fun (hierarchies, occurrences) ->
  compile_to_json ~output ~occurrences ~wrap ~simplified hierarchies
