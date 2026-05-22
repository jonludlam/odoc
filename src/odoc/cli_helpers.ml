open Odoc_utils
module List = ListLabels
open Cmdliner

let convert_syntax : Odoc_document.Renderer.syntax Arg.conv =
  let syntax_parser str =
    match str with
    | "ml" | "ocaml" -> Ok Odoc_document.Renderer.OCaml
    | "re" | "reason" -> Ok Odoc_document.Renderer.Reason
    | s -> Error (Printf.sprintf "Unknown syntax '%s'" s)
  in
  let syntax_printer fmt syntax =
    Format.pp_print_string fmt (Odoc_document.Renderer.string_of_syntax syntax)
  in
  Arg.conv' (syntax_parser, syntax_printer)

let convert_directory ?(create = false) () : Fs.Directory.t Arg.conv =
  let dir_parser, dir_printer =
    (Arg.conv_parser Arg.string, Arg.conv_printer Arg.string)
  in
  let odoc_dir_parser str =
    let () = if create then Fs.Directory.(mkdir_p (of_string str)) in
    match dir_parser str with
    | Ok res -> Ok (Fs.Directory.of_string res)
    | Error (`Msg e) -> Error e
  in
  let odoc_dir_printer fmt dir = dir_printer fmt (Fs.Directory.to_string dir) in
  Arg.conv' (odoc_dir_parser, odoc_dir_printer)

let convert_fpath =
  let parse inp =
    match Arg.(conv_parser file) inp with
    | Ok s -> Ok (Fs.File.of_string s)
    | Error _ as e -> e
  and print = Fpath.pp in
  Arg.conv (parse, print)

let convert_named_root =
  let parse inp =
    match String.cuts inp ~sep:":" with
    | [ s1; s2 ] -> Ok (s1, Fs.Directory.of_string s2)
    | _ -> Error (`Msg "")
  in
  let print ppf (s, t) =
    Format.fprintf ppf "%s:%s" s (Fs.Directory.to_string t)
  in
  Arg.conv (parse, print)

let handle_error = function
  | Ok () -> ()
  | Error (`Cli_error msg) ->
      Printf.eprintf "%s\n%!" msg;
      exit 2
  | Error (`Msg msg) ->
      Printf.eprintf "ERROR: %s\n%!" msg;
      exit 1

module Antichain = struct
  let absolute_normalization p =
    let p =
      if Fpath.is_rel p then Fpath.( // ) (Fpath.v (Sys.getcwd ())) p else p
    in
    Fpath.normalize p

  let check ~opt l =
    let l =
      List.map
        ~f:(fun p -> p |> Fs.Directory.to_fpath |> absolute_normalization)
        l
    in
    let rec check = function
      | [] -> true
      | p1 :: rest ->
          List.for_all
            ~f:(fun p2 ->
              (not (Fpath.is_prefix p1 p2)) && not (Fpath.is_prefix p2 p1))
            rest
          && check rest
    in
    if check l then Ok ()
    else
      let msg =
        Format.sprintf "Paths given to all %s options must be disjoint" opt
      in
      Error (`Msg msg)
end

let docs = "ARGUMENTS"

let odoc_file_directories =
  let doc =
    "Where to look for required $(i,.odoc) files. Can be present several times."
  in
  Arg.(
    value
    & opt_all (convert_directory ()) []
    & info ~docs ~docv:"DIR" ~doc [ "I" ])

let hidden =
  let doc =
    "Mark the unit as hidden. (Useful for files included in module packs)."
  in
  Arg.(value & flag & info ~docs ~doc [ "hidden" ])

let extra_suffix =
  let doc =
    "Extra suffix to append to generated filenames. This is intended for \
     expect tests to use."
  in
  let default = None in
  Arg.(
    value
    & opt (some string) default
    & info ~docv:"SUFFIX" ~doc [ "extra-suffix" ])

let warnings_options =
  let warn_error =
    let doc = "Turn warnings into errors." in
    let env =
      Cmd.Env.info "ODOC_WARN_ERROR" ~doc:(doc ^ " See option $(opt).")
    in
    Arg.(value & flag & info ~docs ~doc ~env [ "warn-error" ])
  in
  let print_warnings =
    let doc =
      "Whether warnings should be printed to stderr. See the $(b,errors) \
       command."
    in
    let env = Cmd.Env.info "ODOC_PRINT_WARNINGS" ~doc in
    Arg.(value & opt bool true & info ~docs ~doc ~env [ "print-warnings" ])
  in
  let enable_missing_root_warning =
    let doc =
      "Produce a warning when a root is missing. This is usually a build \
       system problem so is disabled for users by default."
    in
    let env = Cmd.Env.info "ODOC_ENABLE_MISSING_ROOT_WARNING" ~doc in
    Arg.(value & flag & info ~docs ~doc ~env [ "enable-missing-root-warning" ])
  in
  let warnings_tag =
    let doc =
      "Warnings tag. This is useful when you want to declare that warnings \
       that would be generated resolving the references defined in this unit \
       should be ignored if they end up in expansions in other units. If this \
       option is passed, link-time warnings will be suppressed unless the link \
       command is passed the tag via the --warnings-tags parameter. A suitable \
       tag would be the name of the package."
    in
    let env = Cmd.Env.info "ODOC_WARNINGS_TAG" ~doc in
    Arg.(
      value & opt (some string) None & info ~docs ~doc ~env [ "warnings-tag" ])
  in
  Term.(
    const
      (fun warn_error print_warnings enable_missing_root_warning warnings_tag ->
        Odoc_model.Error.enable_missing_root_warning :=
          enable_missing_root_warning;
        { Odoc_model.Error.warn_error; print_warnings; warnings_tag })
    $ warn_error $ print_warnings $ enable_missing_root_warning $ warnings_tag)

let dst ?create () =
  let doc = "Output directory where the HTML tree is expected to be saved." in
  Arg.(
    required
    & opt (some (convert_directory ?create ())) None
    & info ~docs ~docv:"DIR" ~doc [ "o"; "output-dir" ])

let open_modules =
  let doc =
    "Initially open module. Can be used more than once. Defaults to 'Stdlib'"
  in
  let default = [ "Stdlib" ] in
  Arg.(value & opt_all string default & info ~docv:"MODULE" ~doc [ "open" ])
