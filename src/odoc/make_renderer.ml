open Odoc_utils
open ResultMonad
module List = ListLabels
open Cmdliner

module type S = sig
  type args

  val renderer : args Odoc_document.Renderer.t

  val extra_args : args Cmdliner.Term.t
end

module Make_with_names (R : S) (N : sig
  val process_name : string

  val generate_name : string

  val generate_source_name : string

  val generate_asset_name : string

  val targets_name : string

  val targets_source_name : string
end) : sig
  val process : docs:string -> unit Term.t * Cmd.info

  val targets : docs:string -> unit Term.t * Cmd.info

  val targets_source : docs:string -> unit Term.t * Cmd.info

  val generate : docs:string -> unit Term.t * Cmd.info

  val generate_source : docs:string -> unit Term.t * Cmd.info

  val generate_asset : docs:string -> unit Term.t * Cmd.info
end = struct
  let input_odoc =
    let doc = "Input file." in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE.odoc" [])

  let input_odocl =
    let doc = "Input file." in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE.odocl" [])

  let input_odocl_list =
    let doc = "Input file(s)." in
    Arg.(non_empty & pos_all file [] & info ~doc ~docv:"FILE.odocl" [])

  module Process = struct
    let process extra _hidden directories output_dir syntax input_file
        warnings_options =
      let resolver =
        Resolver.create ~important_digests:false ~directories ~open_modules:[]
          ~roots:None
      in
      let file = Fs.File.of_string input_file in
      Rendering.render_odoc ~renderer:R.renderer ~resolver ~warnings_options
        ~syntax ~output:output_dir extra file

    let cmd =
      let syntax =
        let doc = "Available options: ml | re" in
        let env = Cmd.Env.info "ODOC_SYNTAX" in
        Arg.(
          value
          & opt Cli_helpers.convert_syntax Odoc_document.Renderer.OCaml
            @@ info ~docv:"SYNTAX" ~doc ~env [ "syntax" ])
      in
      Term.(
        const Cli_helpers.handle_error
        $ (const process $ R.extra_args $ Cli_helpers.hidden
         $ Cli_helpers.odoc_file_directories
         $ Cli_helpers.dst ~create:true ()
         $ syntax $ input_odoc $ Cli_helpers.warnings_options))

    let info ~docs =
      let doc =
        Format.sprintf
          "Render %s files from a $(i,.odoc). $(i,link) then $(i,%s) \
           should be used instead."
          R.renderer.name N.generate_name
      in
      Cmd.info ~docs ~doc N.process_name
  end

  let process ~docs = Process.(cmd, info ~docs)

  module Generate = struct
    let generate extra _hidden output_dir syntax extra_suffix input_files
        warnings_options sidebar =
      let process_file input_file =
        let file = Fs.File.of_string input_file in
        Rendering.generate_odoc ~renderer:R.renderer ~warnings_options ~syntax
          ~output:output_dir ~extra_suffix ~sidebar extra file
      in
      List.fold_left
        ~f:(fun acc input_file -> acc >>= fun () -> process_file input_file)
        ~init:(Ok ()) input_files

    let sidebar =
      let doc = "A .odoc-index file, used eg to generate the sidebar." in
      Arg.(
        value
        & opt (some Cli_helpers.convert_fpath) None
        & info [ "sidebar" ] ~doc ~docv:"FILE.odoc-sidebar")

    let cmd =
      let syntax =
        let doc = "Available options: ml | re" in
        let env = Cmd.Env.info "ODOC_SYNTAX" in
        Arg.(
          value
          & opt Cli_helpers.convert_syntax Odoc_document.Renderer.OCaml
            @@ info ~docv:"SYNTAX" ~doc ~env [ "syntax" ])
      in
      Term.(
        const Cli_helpers.handle_error
        $ (const generate $ R.extra_args $ Cli_helpers.hidden
         $ Cli_helpers.dst ~create:true ()
         $ syntax $ Cli_helpers.extra_suffix $ input_odocl_list
         $ Cli_helpers.warnings_options $ sidebar))

    let info ~docs =
      let doc =
        Format.sprintf "Generate %s files from one or more $(i,.odocl) files."
          R.renderer.name
      in
      Cmd.info ~docs ~doc N.generate_name
  end

  let generate ~docs = Generate.(cmd, info ~docs)

  module Generate_source = struct
    let generate extra output_dir syntax extra_suffix input_file
        warnings_options source_file sidebar =
      Rendering.generate_source_odoc ~renderer:R.renderer ~warnings_options
        ~syntax ~output:output_dir ~extra_suffix ~source_file ~sidebar extra
        input_file

    let input_odocl =
      let doc = "Linked implementation file." in
      Arg.(
        required
        & opt (some Cli_helpers.convert_fpath) None
        & info [ "impl" ] ~doc ~docv:"impl-FILE.odocl")

    let source_file =
      let doc = "Source code for the implementation unit." in
      Arg.(
        required
        & pos 0 (some Cli_helpers.convert_fpath) None
        & info ~doc ~docv:"FILE.ml" [])

    let cmd =
      let syntax =
        let doc = "Available options: ml | re" in
        let env = Cmd.Env.info "ODOC_SYNTAX" in
        Arg.(
          value
          & opt Cli_helpers.convert_syntax Odoc_document.Renderer.OCaml
            @@ info ~docv:"SYNTAX" ~doc ~env [ "syntax" ])
      in
      let sidebar = Generate.sidebar in
      Term.(
        const Cli_helpers.handle_error
        $ (const generate $ R.extra_args
         $ Cli_helpers.dst ~create:true ()
         $ syntax $ Cli_helpers.extra_suffix $ input_odocl
         $ Cli_helpers.warnings_options $ source_file $ sidebar))

    let info ~docs =
      let doc =
        Format.sprintf "Generate %s files from a $(i,impl-*.odocl)."
          R.renderer.name
      in
      Cmd.info ~docs ~doc N.generate_source_name
  end

  let generate_source ~docs = Generate_source.(cmd, info ~docs)

  module Generate_asset = struct
    let generate extra output_dir extra_suffix input_file warnings_options
        asset_file =
      Rendering.generate_asset_odoc ~renderer:R.renderer ~warnings_options
        ~output:output_dir ~extra_suffix ~asset_file extra input_file

    let input_odocl =
      let doc = "Odoc asset unit." in
      Arg.(
        required
        & opt (some Cli_helpers.convert_fpath) None
        & info [ "asset-unit" ] ~doc ~docv:"asset-FILE.odocl")

    let asset_file =
      let doc = "The asset file" in
      Arg.(
        required
        & pos 0 (some Cli_helpers.convert_fpath) None
        & info ~doc ~docv:"FILE.ext" [])

    let cmd =
      Term.(
        const Cli_helpers.handle_error
        $ (const generate $ R.extra_args
         $ Cli_helpers.dst ~create:true ()
         $ Cli_helpers.extra_suffix $ input_odocl
         $ Cli_helpers.warnings_options $ asset_file))

    let info ~docs =
      let doc =
        Format.sprintf "Generate %s files from a $(i,impl-*.odocl)."
          R.renderer.name
      in
      Cmd.info ~docs ~doc N.generate_asset_name
  end

  let generate_asset ~docs = Generate_asset.(cmd, info ~docs)

  module Targets = struct
    let list_targets output_dir directories extra odoc_file =
      let odoc_file = Fs.File.of_string odoc_file in
      let resolver =
        Resolver.create ~important_digests:false ~directories ~open_modules:[]
          ~roots:None
      in
      let warnings_options =
        {
          Odoc_model.Error.warn_error = false;
          print_warnings = false;
          warnings_tag = None;
        }
      in
      Rendering.targets_odoc ~resolver ~warnings_options ~syntax:OCaml
        ~renderer:R.renderer ~output:output_dir ~extra odoc_file

    let back_compat =
      let doc =
        "For backwards compatibility when processing $(i,.odoc) rather than \
         $(i,.odocl) files."
      in
      Arg.(
        value
        & opt_all (Cli_helpers.convert_directory ()) []
        & info ~docs:Cli_helpers.docs ~docv:"DIR" ~doc [ "I" ])

    let cmd =
      Term.(
        const Cli_helpers.handle_error
        $ (const list_targets $ Cli_helpers.dst () $ back_compat $ R.extra_args
         $ input_odocl))

    let info ~docs =
      let doc =
        Format.sprintf
          "Print the files that would be generated by $(i,%s)."
          N.generate_name
      in
      Cmd.info N.targets_name ~docs ~doc
  end

  let targets ~docs = Targets.(cmd, info ~docs)

  module Targets_source = struct
    let list_targets output_dir source_file extra odoc_file =
      let warnings_options =
        {
          Odoc_model.Error.warn_error = false;
          print_warnings = false;
          warnings_tag = None;
        }
      in
      Rendering.targets_source_odoc ~warnings_options ~syntax:OCaml
        ~renderer:R.renderer ~output:output_dir ~extra ~source_file odoc_file

    let source_file = Generate_source.source_file
    let input_odocl = Generate_source.input_odocl

    let cmd =
      Term.(
        const Cli_helpers.handle_error
        $ (const list_targets $ Cli_helpers.dst () $ source_file $ R.extra_args
         $ input_odocl))

    let info ~docs =
      let doc =
        Format.sprintf
          "Print the files that would be generated by $(i,%s)."
          N.generate_source_name
      in
      Cmd.info N.targets_source_name ~docs ~doc
  end

  let targets_source ~docs = Targets_source.(cmd, info ~docs)
end

module Make (R : S) =
  Make_with_names
    (R)
    (struct
      let process_name = R.renderer.name
      let generate_name = R.renderer.name ^ "-generate"
      let generate_source_name = R.renderer.name ^ "-generate-source"
      let generate_asset_name = R.renderer.name ^ "-generate-asset"
      let targets_name = R.renderer.name ^ "-targets"
      let targets_source_name = R.renderer.name ^ "-targets-source"
    end)
