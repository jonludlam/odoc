(* CR-someday trefis: the "deps" and "targets" subcommands currently output
   their result on stdout.
   It would make the interaction with jenga nicer if we could specify a file to
   output the result to. *)

open Odoc_utils
open ResultMonad
module List = ListLabels
open Odoc_odoc
open Cmdliner

let convert_directory = Cli_helpers.convert_directory
let convert_fpath = Cli_helpers.convert_fpath
let convert_named_root = Cli_helpers.convert_named_root
let handle_error = Cli_helpers.handle_error
module Antichain = Cli_helpers.Antichain
let docs = Cli_helpers.docs
let odoc_file_directories = Cli_helpers.odoc_file_directories
let hidden = Cli_helpers.hidden
let warnings_options = Cli_helpers.warnings_options
let dst = Cli_helpers.dst
let open_modules = Cli_helpers.open_modules

module Compile : sig
  val output_file : dst:string option -> input:Fs.file -> Fs.file

  val input : string Term.t

  val dst : string option Term.t

  val cmd : unit Term.t

  val info : docs:string -> Cmd.info
end = struct
  let has_page_prefix file =
    file |> Fs.File.basename |> Fs.File.to_string
    |> String.starts_with ~prefix:"page-"

  let unique_id =
    let doc = "For debugging use" in
    Arg.(value & opt (some string) None & info ~doc ~docv:"ID" [ "unique-id" ])

  let output_file ~dst ~input =
    match dst with
    | Some file ->
        let output = Fs.File.of_string file in
        if Fs.File.has_ext ".mld" input && not (has_page_prefix output) then (
          Printf.eprintf
            "ERROR: the name of the .odoc file produced from a .mld must start \
             with 'page-'\n\
             %!";
          exit 1);
        output
    | None ->
        let output =
          if Fs.File.has_ext ".mld" input && not (has_page_prefix input) then
            let directory = Fs.File.dirname input in
            let name = Fs.File.basename input in
            let name = "page-" ^ Fs.File.to_string name in
            Fs.File.create ~directory ~name
          else input
        in
        Fs.File.(set_ext ".odoc" output)

  let compile hidden directories resolve_fwd_refs dst output_dir package_opt
      parent_name_opt parent_id_opt open_modules children input warnings_options
      unique_id short_title =
    let _ =
      match unique_id with
      | Some id -> Odoc_model.Names.set_unique_ident id
      | None -> ()
    in
    let resolver =
      Resolver.create ~important_digests:(not resolve_fwd_refs) ~directories
        ~open_modules ~roots:None
    in
    let input = Fs.File.of_string input in
    let output = output_file ~dst ~input in
    let cli_spec =
      let error message = Error (`Cli_error message) in
      match
        (parent_name_opt, package_opt, parent_id_opt, children, output_dir)
      with
      | Some _, None, None, _, None ->
          Ok (Compile.CliParent { parent = parent_name_opt; children; output })
      | None, Some p, None, [], None ->
          Ok (Compile.CliPackage { package = p; output })
      | None, None, Some p, [], Some output_dir ->
          Ok (Compile.CliParentId { parent_id = p; output_dir })
      | None, None, None, _ :: _, None ->
          Ok (Compile.CliParent { parent = None; output; children })
      | None, None, None, [], None -> Ok (Compile.CliNoParent output)
      | Some _, Some _, _, _, _ ->
          error "Either --package or --parent should be specified, not both."
      | _, Some _, Some _, _, _ ->
          error "Either --package or --parent-id should be specified, not both."
      | Some _, _, Some _, _, _ ->
          error "Either --parent or --parent-id should be specified, not both."
      | _, _, None, _, Some _ ->
          error "--output-dir can only be passed with --parent-id."
      | None, Some _, _, _ :: _, _ ->
          error "--child cannot be passed with --package."
      | None, _, Some _, _ :: _, _ ->
          error "--child cannot be passed with --parent-id."
      | _, _, Some _, _, None ->
          error "--output-dir is required when passing --parent-id."
    in
    cli_spec >>= fun cli_spec ->
    Fs.Directory.mkdir_p (Fs.File.dirname output);
    Compile.compile ~resolver ~cli_spec ~hidden ~warnings_options ~short_title
      input

  let input =
    let doc = "Input $(i,.cmti), $(i,.cmt), $(i,.cmi) or $(i,.mld) file." in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])

  let dst =
    let doc =
      "Output file path. Non-existing intermediate directories are created. If \
       absent outputs a $(i,BASE.odoc) file in the same directory as the input \
       file where $(i,BASE) is the basename of the input file. For mld files \
       the \"page-\" prefix will be added if not already present in the input \
       basename."
    in
    Arg.(value & opt (some string) None & info ~docs ~docv:"PATH" ~doc [ "o" ])

  let output_dir =
    let doc = "Output file directory. " in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"PATH" ~doc [ "output-dir" ])

  let children =
    let doc =
      "Specify the $(i,.odoc) file as a child. Can be used multiple times. \
       Only applies to mld files."
    in
    let default = [] in
    Arg.(
      value & opt_all string default & info ~docv:"CHILD" ~doc [ "c"; "child" ])

  let cmd =
    let package_opt =
      let doc =
        "Package the input is part of. Deprecated: use '--parent' instead."
      in
      Arg.(
        value
        & opt (some string) None
        & info ~docs ~docv:"PKG" ~doc [ "package"; "pkg" ])
    in
    let parent_opt =
      let doc = "Parent page or subpage." in
      Arg.(
        value
        & opt (some string) None
        & info ~docs ~docv:"PARENT" ~doc [ "parent" ])
    in
    let parent_id_opt =
      let doc = "Parent id." in
      Arg.(
        value
        & opt (some string) None
        & info ~docs ~docv:"PARENT" ~doc [ "parent-id" ])
    in
    let short_title =
      let doc = "Override short_title of an mld file" in
      Arg.(
        value
        & opt (some string) None
        & info ~docs ~docv:"TITLE" ~doc [ "short-title" ])
    in
    let resolve_fwd_refs =
      let doc = "Try resolving forward references." in
      Arg.(value & flag & info ~doc [ "r"; "resolve-fwd-refs" ])
    in
    Term.(
      const handle_error
      $ (const compile $ hidden $ odoc_file_directories $ resolve_fwd_refs $ dst
       $ output_dir $ package_opt $ parent_opt $ parent_id_opt $ open_modules
       $ children $ input $ warnings_options $ unique_id $ short_title))

  let info ~docs =
    let man =
      [
        `S "DEPENDENCIES";
        `P
          "Dependencies between compilation units is the same as while \
           compiling the initial OCaml modules.";
        `P "Mld pages don't have any dependency.";
      ]
    in
    let doc =
      "Compile a $(i,.cmti), $(i,.cmt), $(i,.cmi) or $(i,.mld) file to an \
       $(i,.odoc) file."
    in
    Cmd.info "compile" ~docs ~doc ~man
end

module Compile_asset = struct
  let compile_asset parent_id name output_dir =
    Odoc_odoc.Asset.compile ~parent_id ~name ~output_dir

  let output_dir =
    let doc = "Output file directory. " in
    Arg.(
      required
      & opt (some string) None
      & info ~docs ~docv:"PATH" ~doc [ "output-dir" ])

  let cmd =
    let asset_name =
      let doc = "Name of the asset." in
      Arg.(
        required
        & opt (some string) None
        & info ~docs ~docv:"NAME" ~doc [ "name" ])
    in
    let parent_id =
      let doc = "Parent id." in
      Arg.(
        required
        & opt (some string) None
        & info ~docs ~docv:"PARENT" ~doc [ "parent-id" ])
    in
    Term.(
      const handle_error
      $ (const compile_asset $ parent_id $ asset_name $ output_dir))

  let info ~docs =
    let man =
      [
        `S "DEPENDENCIES";
        `P
          "There are no dependency for compile assets, in particular you do \
           not need the asset itself at this stage.";
      ]
    in
    let doc = "Declare the name of an asset." in
    Cmd.info "compile-asset" ~docs ~doc ~man
end

module Compile_impl = struct
  let prefix = "impl-"

  let output_dir =
    let doc = "Output file directory. " in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"PATH" ~doc [ "output-dir" ])

  let output_file output_dir parent_id input =
    let name =
      Fs.File.basename input |> Fpath.set_ext "odoc" |> Fs.File.to_string
      |> String.uncapitalize_ascii
    in
    let name = prefix ^ name in

    let dir = Fpath.(append output_dir parent_id) in
    Fs.File.create
      ~directory:(Fpath.to_string dir |> Fs.Directory.of_string)
      ~name

  let compile_impl directories output_dir parent_id source_id input
      warnings_options =
    let input = Fs.File.of_string input in
    let output_dir =
      match output_dir with Some x -> Fpath.v x | None -> Fpath.v "."
    in
    let output =
      output_file output_dir
        (match parent_id with Some x -> Fpath.v x | None -> Fpath.v ".")
        input
    in
    let resolver =
      Resolver.create ~important_digests:true ~directories ~open_modules:[]
        ~roots:None
    in
    Source.compile ~resolver ~source_id ~output ~warnings_options input

  let cmd =
    let input =
      let doc = "Input $(i,.cmt) file." in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])
    in
    let source_id =
      let doc = "The id of the source file" in
      Arg.(
        value
        & opt (some string) None
        & info [ "source-id" ] ~doc ~docv:"/path/to/source.ml")
    in
    let parent_id =
      let doc = "The parent id of the implementation" in
      Arg.(
        value
        & opt (some string) None
        & info [ "parent-id" ] ~doc ~docv:"/path/to/library")
    in

    Term.(
      const handle_error
      $ (const compile_impl $ odoc_file_directories $ output_dir $ parent_id
       $ source_id $ input $ warnings_options))

  let info ~docs =
    let doc =
      "(EXPERIMENTAL) Compile a $(i,NAME.cmt) file to a $(i,src-NAME.odoc) \
       containing the implementation information needed by odoc for the \
       compilation unit."
    in
    Cmd.info "compile-impl" ~docs ~doc
end

module Indexing = struct
  let output_file ~dst =
    match dst with
    | Some file when not (Fpath.has_ext "odoc-index" (Fpath.v file)) ->
        Error
          (`Msg
             "When generating a binary index, the output must have a \
              .odoc-index file extension")
    | Some file -> Ok (Fs.File.of_string file)
    | None -> Ok (Fs.File.of_string "index.odoc-index")

  let index dst warnings_options roots inputs_in_file inputs occurrences =
    output_file ~dst >>= fun output ->
    Indexing.compile ~output ~warnings_options ~roots ~occurrences
      ~inputs_in_file ~odocls:inputs

  let cmd =
    let dst =
      let doc =
        "Output file path. Non-existing intermediate directories are created. \
         Defaults to index.odoc-index (the .odoc-index file extension is \
         mandatory)."
      in
      Arg.(
        value & opt (some string) None & info ~docs ~docv:"PATH" ~doc [ "o" ])
    in
    let occurrences =
      let doc = "Occurrence file." in
      Arg.(
        value
        & opt (some convert_fpath) None
        & info ~docs ~docv:"PATH" ~doc [ "occurrences" ])
    in
    let inputs_in_file =
      let doc =
        "Input text file containing a line-separated list of paths to .odocl \
         files to index."
      in
      Arg.(
        value & opt_all convert_fpath []
        & info ~doc ~docv:"FILE" [ "file-list" ])
    in
    let inputs =
      let doc = ".odocl file to index" in
      Arg.(value & pos_all convert_fpath [] & info ~doc ~docv:"FILE" [])
    in
    let roots =
      let doc =
        "Specifies a directory PATH containing pages or units that should be \
         included in the sidebar."
      in
      Arg.(
        value
        & opt_all (convert_directory ()) []
        & info ~docs ~docv:"NAME:PATH" ~doc [ "root" ])
    in
    Term.(
      const handle_error
      $ (const index $ dst $ warnings_options $ roots $ inputs_in_file
       $ inputs $ occurrences))

  let info ~docs =
    let doc =
      "Generate a binary index of all identified entries in the .odocl files \
       found in the given directories."
    in
    Cmd.info "compile-index" ~docs ~doc
end

module Sidebar = struct
  let output_file ~dst =
    match dst with
    | Some file when not (Fpath.has_ext "odoc-sidebar" (Fpath.v file)) ->
        Error
          (`Msg
             "When generating sidebar, the output must have a .odoc-sidebar \
              file extension")
    | Some file -> Ok (Fs.File.of_string file)
    | None -> Ok (Fs.File.of_string "sidebar.odoc-sidebar")

  let generate dst warnings_options input =
    output_file ~dst >>= fun output ->
    Sidebar.generate ~output ~warnings_options ~index:input

  let cmd =
    let dst =
      let doc =
        "Output file path. Non-existing intermediate directories are created. \
         Defaults to sidebar.odoc-sidebar."
      in
      Arg.(
        value & opt (some string) None & info ~docs ~docv:"PATH" ~doc [ "o" ])
    in
    let inputs =
      let doc = ".odoc-index file to generate a value from" in
      Arg.(
        required & pos 0 (some convert_fpath) None & info ~doc ~docv:"FILE" [])
    in
    Term.(
      const handle_error
      $ (const generate $ dst $ warnings_options $ inputs))

  let info ~docs =
    let doc = "Generate a sidebar from an index file." in
    Cmd.info "sidebar-generate" ~docs ~doc
end

module Odoc_link : sig
  val cmd : unit Term.t

  val info : docs:string -> Cmd.info
end = struct
  let get_output_file ~output_file ~input =
    match output_file with
    | Some file -> Fs.File.of_string file
    | None -> Fs.File.(set_ext ".odocl" input)

  (** Find the package/library name the output is part of *)
  let find_root_of_input l o =
    let l =
      List.map
        ~f:(fun (x, p) ->
          (x, p, p |> Fs.Directory.to_fpath |> Antichain.absolute_normalization))
        l
    in
    let o = Antichain.absolute_normalization o in
    match l with
    | [] -> None
    | _ ->
        Odoc_utils.List.find_map
          (fun (root, orig_path, norm_path) ->
            if Fpath.is_prefix norm_path o then Some (root, orig_path) else None)
          l

  let current_library_of_input lib_roots input =
    find_root_of_input lib_roots input

  (** Checks if the package specified with [--current-package] is consistent
      with the pages roots and with the output path for pages. *)
  let validate_current_package ?detected_package page_roots current_package =
    match (current_package, detected_package) with
    | Some curpkgnane, Some (detected_package, _)
      when detected_package <> curpkgnane ->
        Error
          (`Msg
             "The package name specified with --current-package is not \
              consistent with the packages passed as a -P")
    | _, (Some _ as r) (* we have equality or only detected package *) -> Ok r
    | None, None -> Ok None
    | Some given, None -> (
        try Ok (Some (given, List.assoc given page_roots))
        with Not_found ->
          Error
            (`Msg
               "The package name specified with --current-package do not match \
                any package passed as a -P"))

  let find_current_package ~current_package page_roots input =
    let detected_package = find_root_of_input page_roots input in
    validate_current_package ?detected_package page_roots current_package

  let warnings_tags =
    let doc =
      "Filter warnings that were compiled with a tag that is not in the list \
       of --warnings-tags passed."
    in
    let env = Cmd.Env.info "ODOC_WARNINGS_TAGS" ~doc in
    Arg.(value & opt_all string [] & info ~docs ~doc ~env [ "warnings-tags" ])

  let link directories page_roots lib_roots input_file output_file
      current_package warnings_options open_modules custom_layout warnings_tags
      =
    let input = Fs.File.of_string input_file in
    let output = get_output_file ~output_file ~input in
    let check () =
      if not custom_layout then
        Antichain.check (page_roots |> List.map ~f:snd) ~opt:"-P" >>= fun () ->
        Antichain.check (lib_roots |> List.map ~f:snd) ~opt:"-L"
      else Ok ()
    in
    check () >>= fun () ->
    let current_lib = current_library_of_input lib_roots input in
    find_current_package ~current_package page_roots input
    >>= fun current_package ->
    let current_dir = Fs.File.dirname input in
    let roots =
      Some
        {
          Resolver.page_roots;
          lib_roots;
          current_lib;
          current_package;
          current_dir;
        }
    in

    let resolver =
      Resolver.create ~important_digests:false ~directories ~open_modules ~roots
    in
    match
      Odoc_link.from_odoc ~resolver ~warnings_options ~warnings_tags input
        output
    with
    | Error _ as e -> e
    | Ok _ -> Ok ()

  let dst =
    let doc =
      "Output file path. Non-existing intermediate directories are created. If \
       absent outputs a $(i,.odocl) file in the same directory as the input \
       file with the same basename."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"PATH.odocl" ~doc [ "o" ])

  let page_roots =
    let doc =
      "Specifies a directory DIR containing pages that can be referenced by \
       {!/pkgname/pagename}. A pkgname can be specified in the -P command only \
       once. All the trees specified by this option and -L must be disjoint."
    in
    Arg.(
      value
      & opt_all convert_named_root []
      & info ~docs ~docv:"pkgname:DIR" ~doc [ "P" ])

  let lib_roots =
    let doc =
      "Specifies a library called libname containing the modules in directory \
       DIR. Modules can be referenced both using the flat module namespace \
       {!Module} and the absolute reference {!/libname/Module}. All the trees \
       specified by this option and -P must be disjoint."
    in
    Arg.(
      value
      & opt_all convert_named_root []
      & info ~docs ~docv:"libname:DIR" ~doc [ "L" ])

  let current_package =
    let doc =
      "Specify the current package name. The matching page root specified with \
       -P is used to resolve references using the '//' syntax. A  \
       corresponding -P option must be passed."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"pkgname" ~doc [ "current-package" ])

  let custom_layout =
    let doc =
      "Signal that a custom layout is being used. This disables the checks \
       that the library and package paths are disjoint."
    in
    Arg.(value & flag (info ~doc [ "custom-layout" ]))

  let cmd =
    let input =
      let doc = "Input file" in
      Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE.odoc" [])
    in
    Term.(
      const handle_error
      $ (const link $ odoc_file_directories $ page_roots $ lib_roots $ input
       $ dst $ current_package $ warnings_options $ open_modules $ custom_layout
       $ warnings_tags))

  let info ~docs =
    let man =
      [
        `S "DEPENDENCIES";
        `P
          "Any link step depends on the result of all the compile results that \
           could potentially be needed to resolve forward references. A \
           correct approximation is to start linking only after every compile \
           steps are done, passing everything that's possible to $(i,-I). Link \
           steps don't have dependencies between them.";
      ]
    in
    let doc =
      "Second stage of compilation. Link a $(i,.odoc) into a $(i,.odocl)."
    in
    Cmd.info ~docs ~doc ~man "link"
end

module Odoc_latex_url : sig
  val cmd : unit Term.t

  val info : docs:string -> Cmd.info
end = struct
  let reference =
    let doc = "The reference to be resolved and whose url to be generated." in
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"REF" [])

  let reference_to_url = Url.reference_to_url_latex

  let cmd =
    Term.(
      const handle_error
      $ (const reference_to_url $ odoc_file_directories $ reference))

  let info ~docs =
    Cmd.info ~docs ~doc:"Resolve a reference and output its corresponding url."
      "latex-url"
end

module Odoc_markdown_cmd = Make_renderer.Make (struct
  type args = Odoc_markdown.Config.t

  let render config _sidebar page = Odoc_markdown.Generator.render ~config page

  let filepath config url = Odoc_markdown.Generator.filepath ~config url

  let extra_args =
    Term.const { Odoc_markdown.Config.root_url = None; allow_html = true }
  let renderer = { Odoc_document.Renderer.name = "markdown"; render; filepath }
end)

module Odoc_manpage = Make_renderer.Make (struct
  type args = unit

  let renderer = Man_page.renderer

  let extra_args = Term.const ()
end)

module Odoc_latex = Make_renderer.Make (struct
  type args = Latex.args

  let renderer = Latex.renderer

  let with_children =
    let doc = "Include children at the end of the page." in
    Arg.(value & opt bool true & info ~docv:"BOOL" ~doc [ "with-children" ])

  let shorten_beyond_depth =
    let doc = "Shorten items beyond the given depth." in
    Arg.(
      value
      & opt (some' int) None
      & info ~docv:"INT" ~doc [ "shorten-beyond-depth" ])

  let remove_functor_arg_link =
    let doc = "Remove link to functor argument." in
    Arg.(
      value & opt bool false
      & info ~docv:"BOOL" ~doc [ "remove-functor-arg-link" ])

  let extra_args =
    let f with_children shorten_beyond_depth remove_functor_arg_link =
      { Latex.with_children; shorten_beyond_depth; remove_functor_arg_link }
    in
    Term.(
      const f $ with_children $ shorten_beyond_depth $ remove_functor_arg_link)
end)

module Depends = struct
  module Compile = struct
    let list_dependencies input_files =
      try
        let deps =
          Depends.for_compile_step (List.map ~f:Fs.File.of_string input_files)
        in
        List.iter
          ~f:(fun t ->
            Printf.printf "%s %s\n" (Depends.Compile.name t)
              (Digest.to_hex @@ Depends.Compile.digest t))
          deps;
        flush stdout
      with Cmi_format.Error e ->
        let msg =
          match e with
          | Not_an_interface file ->
              Printf.sprintf "File %S is not an interface" file
          | Wrong_version_interface (file, v) ->
              Printf.sprintf "File %S is compiled for %s version of OCaml" file
                v
          | Corrupted_interface file ->
              Printf.sprintf "File %S is corrupted" file
        in
        Printf.eprintf "ERROR: %s\n%!" msg;
        exit 1

    let cmd =
      let input =
        let doc = "Input files" in
        Arg.(non_empty & pos_all file [] & info ~doc ~docv:"file.cm{i,t,ti}" [])
      in
      Term.(const list_dependencies $ input)

    let info ~docs =
      Cmd.info "compile-deps" ~docs
        ~doc:
          "List units (with their digest) which needs to be compiled in order \
           to compile this one. The unit itself and its digest is also \
           reported in the output.\n\
           Dependencies between compile steps are the same as when compiling \
           the ocaml modules."
  end

  module Link = struct
    let rec fmt_page pp page =
      match page.Odoc_model.Paths.Identifier.iv with
      | `Page (parent_opt, name) ->
          Format.fprintf pp "%a%a" fmt_parent_opt parent_opt
            Odoc_model.Names.PageName.fmt name
      | `LeafPage (parent_opt, name) ->
          Format.fprintf pp "%a%a" fmt_parent_opt parent_opt
            Odoc_model.Names.PageName.fmt name

    and fmt_parent_opt pp parent_opt =
      match parent_opt with
      | None -> ()
      | Some p -> Format.fprintf pp "%a/" fmt_page p

    let list_dependencies input_file =
      Depends.for_rendering_step (Fs.Directory.of_string input_file)
      >>= fun depends ->
      List.iter depends ~f:(fun (root : Odoc_model.Root.t) ->
          match root.id.iv with
          | `Root (Some p, _) ->
              Format.printf "%a %s %s\n" fmt_page p
                (Odoc_model.Root.Odoc_file.name root.file)
                (Digest.to_hex root.digest)
          | _ ->
              Format.printf "none %s %s\n"
                (Odoc_model.Root.Odoc_file.name root.file)
                (Digest.to_hex root.digest));
      Ok ()

    let cmd =
      let input =
        let doc = "Input directory" in
        Arg.(required & pos 0 (some file) None & info ~doc ~docv:"PKG_DIR" [])
      in
      Term.(const handle_error $ (const list_dependencies $ input))

    let info ~docs =
      Cmd.info "link-deps" ~docs
        ~doc:
          "Lists a subset of the packages and modules which need to be in \
           odoc's load path to link the $(i, odoc) files in the given \
           directory. Additional packages may be required to resolve all \
           references."
  end

end

module Targets = struct
  module Compile = struct
    let list_targets dst input =
      let input = Fs.File.of_string input in
      let output = Compile.output_file ~dst ~input in
      Printf.printf "%s\n" (Fs.File.to_string output);
      flush stdout

    let cmd = Term.(const list_targets $ Compile.dst $ Compile.input)

    let info ~docs =
      Cmd.info "compile-targets" ~docs
        ~doc:
          "Print the name of the file produced by $(i,compile). If $(i,-o) is \
           passed, the same path is printed but error checking is performed."
  end

end

module Occurrences = struct
  let dst_of_string s =
    let f = Fs.File.of_string s in
    if not (Fs.File.has_ext ".odoc-occurrences" f) then
      Error (`Msg "Output file must have '.odoc-occurrences' extension.")
    else Ok f

  module Count = struct
    let count directories dst warnings_options include_hidden =
      dst_of_string dst >>= fun dst ->
      Occurrences.count ~dst ~warnings_options directories include_hidden

    let cmd =
      let dst =
        let doc = "Output file path." in
        Arg.(
          required
          & opt (some string) None
          & info ~docs ~docv:"PATH" ~doc [ "o" ])
      in
      let include_hidden =
        let doc = "Include hidden identifiers in the table" in
        Arg.(value & flag & info ~docs ~doc [ "include-hidden" ])
      in
      let input =
        let doc =
          "Directories to recursively traverse, agregating occurrences from \
           $(i,impl-*.odocl) files. Can be present several times."
        in
        Arg.(
          value
          & pos_all (convert_directory ()) []
          & info ~docs ~docv:"DIR" ~doc [])
      in
      Term.(
        const handle_error
        $ (const count $ input $ dst $ warnings_options $ include_hidden))

    let info ~docs =
      let doc =
        "Generate a hashtable mapping identifiers to number of occurrences, as \
         computed from the implementations of .odocl files found in the given \
         directories."
      in
      Cmd.info "count-occurrences" ~docs ~doc
  end
  module Aggregate = struct
    let index dst files file_list strip_path warnings_options =
      match (files, file_list) with
      | [], [] ->
          Error
            (`Msg
               "At least one of --file-list or a path to a file must be passed \
                to odoc aggregate-occurrences")
      | _ ->
          dst_of_string dst >>= fun dst ->
          Occurrences.aggregate ~dst ~warnings_options ~strip_path files
            file_list

    let cmd =
      let dst =
        let doc = "Output file path." in
        Arg.(
          required
          & opt (some string) None
          & info ~docs ~docv:"PATH" ~doc [ "o" ])
      in
      let inputs_in_file =
        let doc =
          "Input text file containing a line-separated list of paths to files \
           created with count-occurrences."
        in
        Arg.(
          value & opt_all convert_fpath []
          & info ~doc ~docv:"FILE" [ "file-list" ])
      in
      let inputs =
        let doc = "file created with count-occurrences" in
        Arg.(value & pos_all convert_fpath [] & info ~doc ~docv:"FILE" [])
      in
      let strip_path =
        let doc = "Strip package/version information from paths" in
        Arg.(value & flag & info ~doc [ "strip-path" ])
      in
      Term.(
        const handle_error
        $ (const index $ dst $ inputs $ inputs_in_file $ strip_path
         $ warnings_options))

    let info ~docs =
      let doc = "Aggregate hashtables created with odoc count-occurrences." in
      Cmd.info "aggregate-occurrences" ~docs ~doc
  end
end

module Odoc_error = struct
  let errors input =
    let open Odoc_odoc in
    let input = Fs.File.of_string input in
    Odoc_file.load input >>= fun unit ->
    Odoc_model.Error.print_errors unit.warnings;
    Ok ()

  let input =
    let doc = "Input $(i,.odoc) or $(i,.odocl) file" in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])

  let cmd = Term.(const handle_error $ (const errors $ input))

  let info ~docs =
    Cmd.info "errors" ~docs
      ~doc:"Print errors that occurred while compiling or linking."
end

module Classify = struct
  let libdirs =
    let doc = "The directories containing the libraries" in
    Arg.(value & pos_all string [] & info ~doc ~docv:"DIR" [])

  let cmd = Term.(const handle_error $ (const Classify.classify $ libdirs))

  let info ~docs =
    Cmd.info "classify" ~docs
      ~doc:
        "Classify the modules into libraries based on heuristics. Libraries \
         are specified by the --library option."
end

module Extract_code = struct
  let extract dst input line_directives names warnings_options =
    Extract_code.extract ~dst ~input ~line_directives ~names ~warnings_options

  let line_directives =
    let doc = "Whether to include line directives in the output file" in
    Arg.(value & flag & info ~doc [ "line-directives" ])

  let names =
    let doc =
      "From which name(s) of code blocks to extract content. When no names are \
       provided, extract all OCaml code blocks."
    in
    Arg.(value & opt_all string [] & info ~doc [ "name" ])

  let input =
    let doc = "Input $(i,.mld) file." in
    Arg.(required & pos 0 (some file) None & info ~doc ~docv:"FILE" [])

  let dst =
    let doc = "Output file path." in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~docv:"PATH" ~doc [ "o"; "output" ])

  let cmd =
    Term.(
      const handle_error
      $ (const extract $ dst $ input $ line_directives $ names
       $ warnings_options))

  let info ~docs =
    Cmd.info "extract-code" ~docs
      ~doc:
        "Extract code blocks from mld files in order to be able to execute them"
end

let section_pipeline = "COMMANDS: Compilation pipeline"
let section_generators = "COMMANDS: Alternative generators"
let section_support = "COMMANDS: Scripting"
let section_legacy = "COMMANDS: Legacy pipeline"

(** Sections in the order they should appear. *)
let main_page_sections =
  [ section_pipeline; section_generators; section_support; section_legacy ]

let () =
  Printexc.record_backtrace true;
  let cmd_make (term, info) = Cmd.v info term in
  let subcommands =
    List.map ~f:cmd_make
    @@ [
         Occurrences.Count.(cmd, info ~docs:section_pipeline);
         Occurrences.Aggregate.(cmd, info ~docs:section_pipeline);
         Compile.(cmd, info ~docs:section_pipeline);
         Compile_asset.(cmd, info ~docs:section_pipeline);
         Odoc_link.(cmd, info ~docs:section_pipeline);
         Compile_impl.(cmd, info ~docs:section_pipeline);
         Indexing.(cmd, info ~docs:section_pipeline);
         Sidebar.(cmd, info ~docs:section_pipeline);
         Odoc_markdown_cmd.generate ~docs:section_generators;
         Odoc_markdown_cmd.generate_source ~docs:section_generators;
         Odoc_markdown_cmd.targets ~docs:section_support;
         Odoc_manpage.generate ~docs:section_generators;
         Odoc_latex.generate ~docs:section_generators;
         Odoc_latex_url.(cmd, info ~docs:section_support);
         Odoc_error.(cmd, info ~docs:section_support);
         Odoc_manpage.targets ~docs:section_support;
         Odoc_latex.targets ~docs:section_support;
         Depends.Compile.(cmd, info ~docs:section_support);
         Targets.Compile.(cmd, info ~docs:section_support);
         Odoc_manpage.process ~docs:section_legacy;
         Odoc_latex.process ~docs:section_legacy;
         Depends.Link.(cmd, info ~docs:section_legacy);
         Classify.(cmd, info ~docs:section_pipeline);
         Extract_code.(cmd, info ~docs:section_pipeline);
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
    let man =
      (* Show sections in a defined order. *)
      List.map ~f:(fun s -> `S s) main_page_sections
    in
    let default = Term.(const print_default $ const ()) in
    let info = Cmd.info ~man ~version:"%%VERSION%%" "odoc" in
    Cmd.group ~default info subcommands
  in
  match Cmd.eval_value ~err:Format.err_formatter main with
  | Error _ ->
      Format.pp_print_flush Format.err_formatter ();
      exit 2
  | _ -> ()
