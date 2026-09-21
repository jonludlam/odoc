(* Odoc driver *)
open Odoc_driver_lib

let check_packages packages =
  match packages with
  | [] -> ()
  | _ -> (
      match Opam.check packages with
      | Ok () -> ()
      | Error missing ->
          Logs.err (fun m ->
              m "Error: Unknown/uninstalled packages: %a"
                Fmt.Dump.(list string)
                (Util.StringSet.elements missing));
          exit 1)

let run_inner ~odoc_dir ~odocl_dir ~index_dir ~mld_dir ~compile_grep ~link_grep
    ~generate_grep ~index_grep ~remap ~index_mld packages
    {
      Common_args.verbose;
      html_dir;
      stats;
      nb_workers;
      odoc_bin;
      odoc_md_bin;
      generate_json;
      _;
    } () =
  Option.iter (fun odoc_bin -> Odoc.odoc := Bos.Cmd.v odoc_bin) odoc_bin;
  Option.iter
    (fun odoc_md_bin -> Odoc.odoc_md := Bos.Cmd.v odoc_md_bin)
    odoc_md_bin;

  if verbose then Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  check_packages packages;
  Stats.init_nprocs nb_workers;

  let index_mld_content =
    Option.bind index_mld (fun fpath ->
        match Bos.OS.File.read fpath with
        | Ok content -> Some content
        | Error (`Msg msg) ->
            Logs.err (fun m ->
                m "Failed to read index_mld file '%a': %s" Fpath.pp fpath msg);
            exit 1)
  in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = Worker_pool.start_workers env sw nb_workers in
  let all = Packages.of_packages ~packages_dir:None packages in
  let all = Packages.remap_virtual all in
  let extra_paths = Voodoo.empty_extra_paths in

  let remaps =
    if remap then List.concat_map (fun pkg -> pkg.Packages.remaps) all else []
  in

  Logs.app (fun m -> m "Starting the compilation process...");

  let () =
    Eio.Fiber.both
      (fun () ->
        let pkgs =
          let dirs = { Odoc_unit.odoc_dir; odocl_dir; index_dir; mld_dir } in
          Odoc_units_of.packages ~dirs
            ~indices_style:
              (Odoc_units_of.Normal { toplevel_content = index_mld_content })
            ~extra_paths ~remap all
        in
        Compile.init_stats pkgs;
        (* Build package by package, each package's modules finding those of
           its dependencies through the -I path.

           Libraries are compiled in dependency order the way Compile.compile_mod
           orders modules: [compile_lib] compiles a library after the libraries
           it requires, memoised in a table. Library requires form a DAG even
           where packages are mutually dependent (ppx_deriving.api needs ppxlib,
           ppxlib.traverse needs ppx_deriving.runtime), which is why this
           recursion is over libraries rather than packages. A required library
           that no package of this run provides is either already built or an
           alias for other libraries (num for num.core), so its own requires
           are followed.

           Linking a package needs every package in its reference scope, and
           odoc-config.sexp can put a package built later there -- eio points
           at eio_main, odoc at odoc-driver. So before a package is linked,
           whatever its scope names is compiled too. This is the split that
           voodoo mode exposes as --actions compile-only / link-and-gen.

           The top-level index, which belongs to no package, comes last. *)
        let units_by_name =
          List.fold_left
            (fun acc (p : Odoc_unit.pkg) ->
              match p.pkgname with
              | Some name -> Util.StringMap.add name p acc
              | None -> acc)
            Util.StringMap.empty pkgs
        in
        let toplevel =
          List.filter (fun (p : Odoc_unit.pkg) -> p.pkgname = None) pkgs
        in
        let libs_by_name =
          List.fold_left
            (fun acc (p : Odoc_unit.pkg) ->
              List.fold_left
                (fun acc (lib : Odoc_unit.lib) ->
                  Util.StringMap.add lib.lib_name (p, lib) acc)
                acc p.libs)
            Util.StringMap.empty pkgs
        in
        let lib_requires = Packages.lib_requires all in
        let compiled_libs = Hashtbl.create 1000 in
        let rec compile_lib name =
          if not (Hashtbl.mem compiled_libs name) then (
            Hashtbl.add compiled_libs name ();
            Util.StringSet.iter compile_lib (lib_requires name);
            match Util.StringMap.find_opt name libs_by_name with
            | None -> ()
            | Some (pkg, lib) ->
                Logs.debug (fun m -> m "Compiling library %s" name);
                ignore
                  (Compile.compile [ { pkg with libs = [ lib ]; pages = [] } ]))
        in
        let compiled_pkgs = Hashtbl.create 100 in
        let compile_pkg name =
          if not (Hashtbl.mem compiled_pkgs name) then (
            Hashtbl.add compiled_pkgs name ();
            match Util.StringMap.find_opt name units_by_name with
            | None -> ()
            | Some units ->
                List.iter
                  (fun (lib : Odoc_unit.lib) -> compile_lib lib.lib_name)
                  units.libs;
                ignore (Compile.compile [ { units with libs = [] } ]))
        in
        let link_and_generate pkgs =
          (* Libraries sharing an object directory share an odoc directory, so
             their [-L] roots overlap; [--custom-layout] tells odoc that is
             intended. *)
          let linked =
            Compile.link ~warnings_tags:packages ~custom_layout:true pkgs
          in
          Compile.html_generate ~remaps ~generate_json html_dir linked
        in
        List.iter
          (fun (pkg : Packages.t) ->
            compile_pkg pkg.name;
            match Util.StringMap.find_opt pkg.name units_by_name with
            | None -> ()
            | Some units ->
                List.iter (fun (name, _) -> compile_pkg name) units.scope.pages;
                Logs.debug (fun m -> m "Linking %s" pkg.name);
                link_and_generate [ units ];
                Status.file ~html_dir ~pkg ())
          all;
        let _ = Compile.compile toplevel in
        link_and_generate toplevel;
        let _ = Odoc.support_files html_dir in
        Stats.stats.finished <- true;
        ())
      (fun () -> Stats.render_stats env ~generate_json nb_workers)
  in

  Logs.app (fun m ->
      m "Documentation generation complete. Results are in %a" Fpath.pp html_dir);

  let grep_log ty s =
    let open Astring in
    let do_ affix =
      let grep { Cmd_outputs.log_dest; prefix; run } =
        if log_dest = ty then
          let l = run.Run.cmd |> String.concat ~sep:" " in
          if String.is_infix ~affix l then Format.printf "%s: %s\n" prefix l
      in
      List.iter grep !Cmd_outputs.outputs
    in
    Option.iter do_ s
  in
  (* Grep log compile and compile_src commands *)
  grep_log `Compile compile_grep;
  grep_log `Compile_src compile_grep;
  (* Grep log link commands *)
  grep_log `Link link_grep;
  (* Grep log generate commands *)
  grep_log `Generate generate_grep;
  (* Grep log index and co commands *)
  grep_log `Count_occurrences index_grep;
  grep_log `Count_occurrences index_grep;
  grep_log `Index index_grep;

  let maybe_write_header =
    let written = ref false in
    fun () ->
      if not !written then (
        written := true;
        Logs.app (fun m -> m "Output from commands:"))
  in

  List.iter
    (fun { Cmd_outputs.log_dest; prefix; run } ->
      match log_dest with
      | `Link | `Compile ->
          [ run.Run.output; run.Run.errors ]
          |> List.iter @@ fun content ->
             if String.length content = 0 then ()
             else (
               maybe_write_header ();
               Logs.app (fun m -> m "%s" prefix);
               Logs.app (fun m ->
                   m "%s" (String.init (String.length prefix) (fun _ -> '-')));
               let lines = String.split_on_char '\n' content in
               List.iter (fun l -> Logs.app (fun m -> m "%s" l)) lines;
               Logs.app (fun m -> m ""))
      | _ -> ())
    !Cmd_outputs.outputs;

  if stats then Stats.bench_results html_dir

let run dirs compile_grep link_grep generate_grep index_grep remap packages
    index_mld common : unit =
  let fn =
    run_inner ~compile_grep ~link_grep ~generate_grep ~index_grep ~remap
      ~index_mld packages common
  in
  Common_args.with_dirs dirs fn

open Cmdliner

let compile_grep =
  let doc = "Show compile commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "compile-grep" ] ~doc ~docs:Manpage.s_none)

let link_grep =
  let doc = "Show link commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "link-grep" ] ~doc ~docs:Manpage.s_none)

let generate_grep =
  let doc = "Show html-generate commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "html-grep" ] ~doc ~docs:Manpage.s_none)

let index_grep =
  let doc = "Show compile-index commands containing the string" in
  Arg.(
    value
    & opt (some string) None
    & info [ "index-grep" ] ~doc ~docs:Manpage.s_none)

let remap =
  let doc = "Remap paths in non-selected packages to ocaml.org" in
  Arg.(value & flag & info [ "remap" ] ~doc ~docs:Manpage.s_common_options)

let packages = Arg.(value & pos_all string [] & info [] ~docv:"PACKAGES")

let index_mld =
  let doc =
    "Provide an index.mld file to serve as the top-level index of the \
     documentation"
  in
  Arg.(
    value
    & opt (some Common_args.fpath_arg) None
    & info [ "index-mld" ] ~docv:"INDEX" ~doc)

let cmd_term =
  let module A = Common_args in
  Term.(
    const run $ A.dirs_term $ compile_grep $ link_grep $ generate_grep
    $ index_grep $ remap $ packages $ index_mld $ Common_args.term)

let cmd =
  let doc =
    "Documents packages present in your opam switch. This mode will select all \
     opam packages that are dependencies of the selected packages."
  in
  let info = Cmd.info "odoc_driver" ~doc in
  Cmd.v info cmd_term

let _ = exit (Cmd.eval cmd)
