(* ocamlobjinfo *)

open Bos
let ocamlobjinfo = Cmd.v "ocamlobjinfo"

let source_possibilities file =
  let default = [ file ] in
  let generated =
    if String.ends_with ~suffix:"-gen" file then
      let pos = String.length file - 4 in
      [ String.sub file 0 (min pos (String.length file)) ]
    else []
  in
  let pp =
    if String.ends_with ~suffix:".pp.ml" file then
      let pos = String.length file - 5 in
      [ String.sub file 0 (min pos (String.length file)) ^ "ml" ]
    else []
  in
  pp @ default @ generated

let get_source file srcdirs =
  let cmd = Cmd.(ocamlobjinfo % p file) in
  let lines_res =
    Worker_pool.submit ("Ocamlobjinfo " ^ Fpath.to_string file) cmd None
  in
  let lines =
    match lines_res with
    | Ok l -> String.split_on_char '\n' l.output
    | Error e ->
        Logs.err (fun m ->
            m "Error finding source for module %a: %s" Fpath.pp file
              (Printexc.to_string e));
        []
  in
  let f =
    List.filter_map
      (fun line ->
        let affix = "Source file: " in
        if String.starts_with ~prefix:affix line then
          let name =
            String.sub line (String.length affix)
              (String.length line - String.length affix)
          in
          let name = Fpath.(filename (v name)) in
          let possibilities =
            List.map
              (fun dir ->
                List.map
                  (fun poss -> Fpath.(dir / poss))
                  (source_possibilities name))
              srcdirs
            |> List.flatten
          in
          List.find_opt
            (fun f ->
              Logs.debug (fun m -> m "src: checking %a" Fpath.pp f);
              Sys.file_exists (Fpath.to_string f))
            possibilities
        else None)
      lines
  in
  match f with
  | [] -> None
  | x :: _ :: _ ->
      Logs.warn (fun m -> m "Multiple source files found for %a" Fpath.pp file);
      Some x
  | x :: _ -> Some x
