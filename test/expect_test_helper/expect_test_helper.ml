let ends_with_newline s = String.length s > 0 && s.[String.length s - 1] = '\n'

let print ~input ~output =
  print_string "--- input ---\n";
  print_string input;
  if not (ends_with_newline input) then print_char '\n';
  print_string "--- output ---\n";
  print_string output;
  if not (ends_with_newline output) then print_char '\n'

let run groups =
  match Sys.argv with
  | [| _; name |] -> (
      match List.assoc_opt name groups with
      | Some f -> f ()
      | None ->
          prerr_endline ("Unknown group: " ^ name);
          exit 2)
  | _ ->
      prerr_endline
        (Printf.sprintf "usage: %s <group>" (Filename.basename Sys.argv.(0)));
      exit 2
