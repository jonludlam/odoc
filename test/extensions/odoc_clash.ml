(* Extensions that cannot be registered together, or at all, chosen by
   [ODOC_CLASH]. *)

let extension name = Odoc_extension.v ~name

let extensions =
  match Sys.getenv_opt "ODOC_CLASH" with
  | Some "raw-markup" ->
      [ { (extension "one") with raw_markup = [ ("HTML", fun _ _ -> None) ] } ]
  | Some "support-file" ->
      [ { (extension "one") with support_files = [ ("odoc.css", "") ] } ]
  | _ ->
      List.map
        (fun name ->
          { (extension name) with code_blocks = [ ("shout", fun _ _ -> None) ] })
        [ "one"; "two" ]

let () = Odoc_cli.main ~extensions ()
