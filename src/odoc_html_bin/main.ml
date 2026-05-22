open Cmdliner

let () =
  Printexc.record_backtrace true;
  let main =
    let print_default () =
      Printf.printf
        "Available subcommands: (none yet)\nSee --help for more information.\n%!"
    in
    let default = Term.(const print_default $ const ()) in
    let info = Cmd.info ~version:"%%VERSION%%" "odoc-html" in
    Cmd.group ~default info []
  in
  match Cmd.eval_value ~err:Format.err_formatter main with
  | Error _ ->
      Format.pp_print_flush Format.err_formatter ();
      exit 2
  | _ -> ()
