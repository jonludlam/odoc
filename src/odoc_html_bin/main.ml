open Odoc_utils
open ResultMonad
module List = ListLabels
open Odoc_odoc
open Odoc_html_bin
open Cmdliner

module Odoc_html_cmds =
  Make_renderer.Make_with_names
    (struct
      type args = Html_args.args

      let renderer = Html_args.renderer
      let extra_args = Html_args.extra_args
    end)
    (struct
      let process_name = "process"
      let generate_name = "generate"
      let generate_source_name = "generate-source"
      let generate_asset_name = "generate-asset"
      let targets_name = "targets"
      let targets_source_name = "targets-source"
    end)

module Odoc_html_url : sig
  val cmd : unit Term.t

  val info : docs:string -> Cmd.info
end = struct
  let root_url =
    let doc =
      "A string to prepend to the generated relative url. A separating / is \
       added if needed."
    in
    Arg.(value & opt (some string) None & info [ "r"; "root-url" ] ~doc)

  let reference =
    let doc = "The reference to be resolved and whose url to be generated." in
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"REF" [])

  let reference_to_url = Url.reference_to_url_html

  let cmd =
    Term.(
      const Cli_helpers.handle_error
      $ (const reference_to_url $ Html_args.extra_args $ root_url
       $ Cli_helpers.odoc_file_directories $ reference))

  let info ~docs =
    Cmd.info ~docs ~doc:"Resolve a reference and output its corresponding url."
      "url"
end

module Html_fragment_cmd : sig
  val cmd : unit Term.t

  val info : docs:string -> Cmd.info
end = struct
  let html_fragment directories xref_base_uri output_file input_file
      warnings_options =
    let resolver =
      Resolver.create ~important_digests:false ~directories ~open_modules:[]
        ~roots:None
    in
    let input_file = Fs.File.of_string input_file in
    let output_file = Fs.File.of_string output_file in
    let xref_base_uri =
      if xref_base_uri = "" then xref_base_uri
      else
        let last_char = xref_base_uri.[String.length xref_base_uri - 1] in
        if last_char <> '/' then xref_base_uri ^ "/" else xref_base_uri
    in
    Html_fragment.from_mld ~resolver ~xref_base_uri ~output:output_file
      ~warnings_options input_file

  let cmd =
    let output =
      let doc = "Output HTML fragment file." in
      Arg.(
        value & opt string "/dev/stdout"
        & info ~docs:Cli_helpers.docs ~docv:"file.html" ~doc
            [ "o"; "output-file" ])
    in
    let input =
      let doc = "Input documentation page file." in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"file.mld" [])
    in
    let xref_base_uri =
      let doc =
        "Base URI used to resolve cross-references. Set this to the root of \
         the global docset during local development. By default `.' is used."
      in
      Arg.(value & opt string "" & info ~docv:"URI" ~doc [ "xref-base-uri" ])
    in
    Term.(
      const Cli_helpers.handle_error
      $ (const html_fragment $ Cli_helpers.odoc_file_directories $ xref_base_uri
       $ output $ input $ Cli_helpers.warnings_options))

  let info ~docs =
    Cmd.info ~docs ~doc:"Generates an html fragment file from an mld one."
      "fragment"
end

module Support_files_command = struct
  let support_files without_theme output_dir =
    Support_files.write ~without_theme output_dir

  let without_theme =
    let doc = "Don't copy the default theme to output directory." in
    Arg.(value & flag & info ~doc [ "without-theme" ])

  let cmd =
    Term.(const support_files $ without_theme $ Cli_helpers.dst ~create:true ())

  let info ~docs =
    let doc =
      "Copy the support files (e.g. default theme, JavaScript files) to the \
       output directory."
    in
    Cmd.info ~docs ~doc "support-files"
end

module Support_files_targets = struct
  let list_targets without_theme output_directory =
    Support_files.print_filenames ~without_theme output_directory

  let cmd =
    Term.(
      const list_targets
      $ Support_files_command.without_theme
      $ Cli_helpers.dst ())

  let info ~docs =
    Cmd.info "support-files-targets" ~docs
      ~doc:"Lists the names of the files that $(i,odoc-html support-files) outputs."
end

module Sidebar_json = struct
  let output_file ~dst =
    match dst with
    | Some file
      when not
             (Path.has_ext ~ext:"json" file
             || Path.has_ext ~ext:"js" file) ->
        Error
          (`Msg
             "When generating a json sidebar, the output must have a .json or \
              .js file extension")
    | Some file -> Ok (Fs.File.of_string file)
    | None -> Ok (Fs.File.of_string "sidebar.json")

  let compile_to_json ~output sidebar =
    let json = Odoc_html.Sidebar.to_json sidebar in
    let text = Json.to_string json in
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    Io_utils.with_open_out_bin (Fs.File.to_string output) @@ fun oc ->
    Printf.fprintf oc "%s" text

  let generate dst _warnings_options input =
    output_file ~dst >>= fun output ->
    Odoc_file.load_index input >>= fun index ->
    let sidebar = Odoc_document.Sidebar.of_index index in
    Ok (compile_to_json ~output sidebar)

  let cmd =
    let dst =
      let doc =
        "Output file path. Non-existing intermediate directories are created. \
         Defaults to sidebar.json (a .json or .js file extension is \
         required)."
      in
      Arg.(
        value & opt (some string) None
        & info ~docs:Cli_helpers.docs ~docv:"PATH" ~doc [ "o" ])
    in
    let inputs =
      let doc = ".odoc-index file to generate a json sidebar from" in
      Arg.(
        required
        & pos 0 (some Cli_helpers.convert_fpath) None
        & info ~doc ~docv:"FILE" [])
    in
    Term.(
      const Cli_helpers.handle_error
      $ (const generate $ dst $ Cli_helpers.warnings_options $ inputs))

  let info ~docs =
    let doc =
      "Generate a JSON sidebar (consumed by the html search frontend) from an \
       index file."
    in
    Cmd.info "sidebar-generate-json" ~docs ~doc
end

module Compile_index_json = struct
  let output_file ~dst =
    match dst with
    | Some file
      when not
             (Path.has_ext ~ext:"json" file
             || Path.has_ext ~ext:"js" file) ->
        Error
          (`Msg
             "When generating a json index, the output must have a .json or \
              .js file extension")
    | Some file -> Ok (Fs.File.of_string file)
    | None -> Ok (Fs.File.of_string "index.json")

  let index dst warnings_options roots inputs_in_file inputs occurrences
      simplified_json wrap_json =
    output_file ~dst >>= fun output ->
    Odoc_html_bin.Compile_index_json.compile ~output ~warnings_options ~roots
      ~occurrences ~inputs_in_file ~odocls:inputs ~simplified:simplified_json
      ~wrap:wrap_json

  let cmd =
    let dst =
      let doc =
        "Output file path. Non-existing intermediate directories are created. \
         Defaults to index.json (a .json or .js file extension is required)."
      in
      Arg.(
        value & opt (some string) None
        & info ~docs:Cli_helpers.docs ~docv:"PATH" ~doc [ "o" ])
    in
    let occurrences =
      let doc = "Occurrence file." in
      Arg.(
        value
        & opt (some Cli_helpers.convert_fpath) None
        & info ~docs:Cli_helpers.docs ~docv:"PATH" ~doc [ "occurrences" ])
    in
    let inputs_in_file =
      let doc =
        "Input text file containing a line-separated list of paths to .odocl \
         files to index."
      in
      Arg.(
        value & opt_all Cli_helpers.convert_fpath []
        & info ~doc ~docv:"FILE" [ "file-list" ])
    in
    let simplified_json =
      let doc = "Whether to simplify the json output." in
      Arg.(value & flag & info ~doc [ "simplified-json" ])
    in
    let wrap_json =
      let doc =
        "Not intended for general use. Wraps the json output in a JavaScript \
         variable assignment, and assumes the use of fuse.js"
      in
      Arg.(value & flag & info ~doc [ "wrap-json" ])
    in
    let inputs =
      let doc = ".odocl file to index" in
      Arg.(
        value
        & pos_all Cli_helpers.convert_fpath []
        & info ~doc ~docv:"FILE" [])
    in
    let roots =
      let doc =
        "Specifies a directory PATH containing pages or units that should be \
         included in the index."
      in
      Arg.(
        value
        & opt_all (Cli_helpers.convert_directory ()) []
        & info ~docs:Cli_helpers.docs ~docv:"NAME:PATH" ~doc [ "root" ])
    in
    Term.(
      const Cli_helpers.handle_error
      $ (const index $ dst $ Cli_helpers.warnings_options $ roots
       $ inputs_in_file $ inputs $ occurrences $ simplified_json $ wrap_json))

  let info ~docs =
    let doc =
      "Generate a JSON index of all identified entries in the .odocl files \
       found in the given directories."
    in
    Cmd.info "compile-index-json" ~docs ~doc
end

let section_pipeline = "COMMANDS: HTML pipeline"
let section_support = "COMMANDS: Scripting"
let section_legacy = "COMMANDS: Legacy pipeline"

let main_page_sections =
  [ section_pipeline; section_support; section_legacy ]

let () =
  Printexc.record_backtrace true;
  let cmd_make (term, info) = Cmd.v info term in
  let subcommands =
    List.map ~f:cmd_make
    @@ [
         Odoc_html_cmds.generate ~docs:section_pipeline;
         Odoc_html_cmds.generate_source ~docs:section_pipeline;
         Odoc_html_cmds.generate_asset ~docs:section_pipeline;
         Support_files_command.(cmd, info ~docs:section_pipeline);
         Sidebar_json.(cmd, info ~docs:section_pipeline);
         Compile_index_json.(cmd, info ~docs:section_pipeline);
         Odoc_html_url.(cmd, info ~docs:section_support);
         Support_files_targets.(cmd, info ~docs:section_support);
         Odoc_html_cmds.targets ~docs:section_support;
         Odoc_html_cmds.targets_source ~docs:section_support;
         Html_fragment_cmd.(cmd, info ~docs:section_legacy);
         Odoc_html_cmds.process ~docs:section_legacy;
       ]
  in
  let main =
    let print_default () =
      let available_subcommands =
        List.map subcommands ~f:(fun cmd -> Cmd.name cmd)
      in
      Printf.printf
        "Available subcommands: %s\nSee --help for more information.\n%!"
        (String.concat ", " available_subcommands)
    in
    let man = List.map ~f:(fun s -> `S s) main_page_sections in
    let default = Term.(const print_default $ const ()) in
    let info = Cmd.info ~man ~version:"%%VERSION%%" "odoc-html" in
    Cmd.group ~default info subcommands
  in
  match Cmd.eval_value ~err:Format.err_formatter main with
  | Error _ ->
      Format.pp_print_flush Format.err_formatter ();
      exit 2
  | _ -> ()
