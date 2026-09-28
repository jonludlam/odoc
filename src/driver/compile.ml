(* Compile, link and render the units of one package. *)

open Bos
open Eio.Std

let init_stats (pkgs : Odoc_unit.pkg list) =
  let units = List.concat_map Odoc_unit.all_units pkgs in
  let total, total_impl, non_hidden, mlds, assets =
    List.fold_left
      (fun (total, total_impl, non_hidden, mlds, assets) (unit : Odoc_unit.any)
         ->
        let total = match unit.kind with `Intf _ -> total + 1 | _ -> total in
        let total_impl =
          match unit.kind with `Impl _ -> total_impl + 1 | _ -> total_impl
        in
        let assets =
          match unit.kind with `Asset -> assets + 1 | _ -> assets
        in
        let non_hidden =
          match unit.kind with
          | `Intf { hidden = false; _ } -> non_hidden + 1
          | _ -> non_hidden
        in
        let mlds = match unit.kind with `Mld | `Md -> mlds + 1 | _ -> mlds in
        (total, total_impl, non_hidden, mlds, assets))
      (0, 0, 0, 0, 0) units
  in
  let indexes =
    List.length (List.filter (fun (p : Odoc_unit.pkg) -> p.index <> None) pkgs)
  in
  Atomic.set Stats.stats.total_units total;
  Atomic.set Stats.stats.total_impls total_impl;
  Atomic.set Stats.stats.non_hidden_units non_hidden;
  Atomic.set Stats.stats.total_mlds mlds;
  Atomic.set Stats.stats.total_assets assets;
  Atomic.set Stats.stats.total_indexes indexes

(* An implementation of a virtual library ships its modules as [.cmt] files
   only: the interface, and the documentation written in it, belong to the
   virtual library's [.cmti], which shares the module's digest. When the
   virtual library is built in the same run, [Packages.remap_virtual] has
   already substituted the [.cmti]; when it was built earlier (voodoo mode),
   the copy stashed next to its [.odoc] file ([input_copy]) is on the
   implementation's dependency cone, so look for a same-named [.cmti] with the
   right digest along the library's [-I] path. *)
let find_virtual_interface ~includes (unit : Odoc_unit.intf Odoc_unit.t) =
  if not (Fpath.has_ext "cmt" unit.input_file) then unit
  else
    let (`Intf { Odoc_unit.hash; _ }) = unit.kind in
    let name = Fpath.(unit.input_file |> rem_ext |> basename) in
    let candidate dir =
      let cmti = Fpath.(dir / (name ^ ".cmti")) in
      match OS.File.exists cmti with
      | Ok true -> (
          match Odoc.compile_deps cmti with
          | Ok { digest; _ } when digest = hash -> Some cmti
          | _ -> None)
      | _ -> None
    in
    match List.find_map candidate includes with
    | None -> unit
    | Some cmti ->
        Logs.debug (fun m ->
            m "Using %a as the interface of %a" Fpath.pp cmti Fpath.pp
              unit.input_file);
        { unit with input_file = cmti }

(* The interfaces of a library, keyed by digest: a virtual library's interface
   and those of its implementations share one. *)
let by_hash (units : [ Odoc_unit.intf | Odoc_unit.impl ] Odoc_unit.t list) =
  List.fold_left
    (fun acc (u : _ Odoc_unit.t) ->
      match u.kind with
      | `Intf _ as kind ->
          let (`Intf { Odoc_unit.hash; _ }) = kind in
          Util.StringMap.update hash
            (function
              | None -> Some [ { u with kind } ]
              | Some x -> Some ({ u with kind } :: x))
            acc
      | `Impl _ -> acc)
    Util.StringMap.empty units

let compile_lib (pkg : Odoc_unit.pkg) (lib : Odoc_unit.lib) =
  let libs = lib.includes in
  let hashes = by_hash lib.units in
  (* A module is compiled after the modules it imports: [compile_mod] on a
     digest compiles the interfaces with that digest, once, awaiting any
     compilation already under way. An import whose digest is not among this
     library's modules belongs to a library compiled earlier and is found
     through the [-I] path. *)
  let started = Hashtbl.create 100 in
  let rec compile_mod hash =
    match Util.StringMap.find_opt hash hashes with
    | None -> ()
    | Some units ->
        Fiber.List.iter
          (fun (unit : Odoc_unit.intf Odoc_unit.t) ->
            let key = (hash, Odoc.Id.to_string unit.parent_id) in
            match Hashtbl.find_opt started key with
            | Some done_ -> Promise.await done_
            | None ->
                let done_, resolve = Promise.create () in
                Hashtbl.add started key done_;
                compile_intf unit;
                Promise.resolve resolve ())
          units
  and compile_intf (unit : Odoc_unit.intf Odoc_unit.t) =
    let (`Intf { Odoc_unit.deps; _ }) = unit.kind in
    Fiber.List.iter (fun (_, hash) -> compile_mod hash) deps;
    let unit =
      find_virtual_interface ~includes:(Odoc_unit.include_dirs lib) unit
    in
    Odoc.compile ~output_file:unit.odoc_file ~input_file:unit.input_file ~libs
      ~warnings_tag:pkg.pkgname ~parent_id:unit.parent_id
      ~ignore_output:(not unit.enable_warnings);
    (match unit.input_copy with
    | None -> ()
    | Some p -> Util.cp (Fpath.to_string unit.input_file) (Fpath.to_string p));
    Atomic.incr Stats.stats.compiled_units
  in
  let compile (unit : _ Odoc_unit.t) =
    match unit.kind with
    | `Intf { Odoc_unit.hash; _ } -> compile_mod hash
    | `Impl { Odoc_unit.src_id; _ } ->
        Odoc.compile_impl ~output_file:unit.odoc_file
          ~input_file:unit.input_file ~libs ~parent_id:unit.parent_id
          ~source_id:src_id;
        Atomic.incr Stats.stats.compiled_impls
  in
  Fiber.List.iter compile lib.units

let compile_pages (pkg : Odoc_unit.pkg) =
  let compile (unit : _ Odoc_unit.t) =
    match unit.kind with
    | `Mld ->
        Odoc.compile ~output_file:unit.odoc_file ~input_file:unit.input_file
          ~libs:[] ~warnings_tag:None ~parent_id:unit.parent_id
          ~ignore_output:(not unit.enable_warnings);
        Atomic.incr Stats.stats.compiled_mlds
    | `Md ->
        Odoc.compile_md ~output_file:unit.odoc_file ~input_file:unit.input_file
          ~parent_id:unit.parent_id;
        Atomic.incr Stats.stats.compiled_mlds
    | `Asset ->
        Odoc.compile_asset
          ~output_dir:(Odoc_unit.output_root unit)
          ~parent_id:unit.parent_id
          ~name:(Fpath.filename unit.input_file);
        Atomic.incr Stats.stats.compiled_assets
  in
  let lib_pages =
    List.filter_map (fun (l : Odoc_unit.lib) -> l.page) pkg.libs
  in
  Fiber.List.iter compile (pkg.pages @ (lib_pages :> Odoc_unit.page list))

let link ~warnings_tags (pkg : Odoc_unit.pkg) =
  let link ~scope ~includes (c : Odoc_unit.any) =
    let { Odoc_unit.page_roots; lib_roots } = scope in
    match c.kind with
    | `Intf { hidden = true; _ } -> ()
    | _ ->
        (* Libraries sharing an object directory share an odoc directory, so
           their -L roots overlap; --custom-layout tells odoc that is
           intended. *)
        if c.to_output then
          Odoc.link ~input_file:c.odoc_file ~output_file:c.odocl_file
            ~libs:lib_roots ~docs:page_roots ~includes
            ~ignore_output:(not c.enable_warnings) ~warnings_tags
            ?current_package:pkg.pkgname ();
        Atomic.incr
          (match c.kind with
          | `Intf _ -> Stats.stats.linked_units
          | `Impl _ -> Stats.stats.linked_impls
          | `Mld | `Md | `Asset -> Stats.stats.linked_mlds)
  in
  let jobs =
    List.map (fun u -> (pkg.scope, [], u)) (pkg.pages :> Odoc_unit.any list)
    @ List.concat_map
        (fun (lib : Odoc_unit.lib) ->
          (* The page the driver writes for the library shows what the
             library's modules say, so it is linked with their search path:
             odoc then prefers this library's modules to the same-named
             modules of another library in the scope. *)
          let page =
            match lib.page with
            | None -> []
            | Some p -> [ (p :> Odoc_unit.any) ]
          in
          let includes = Odoc_unit.include_dirs lib in
          List.map
            (fun u -> (pkg.scope, includes, u))
            (page @ (lib.units :> Odoc_unit.any list)))
        pkg.libs
  in
  Fiber.List.iter (fun (scope, includes, u) -> link ~scope ~includes u) jobs

(* The index of a package is built from the [.odocl] files of its linked units.
   Listing them explicitly puts the package's pages and modules in one hierarchy
   even though they live in different directories. *)
let index_file_list (pkg : Odoc_unit.pkg) (index : Odoc_unit.index) =
  let inputs =
    Odoc_unit.all_units pkg
    |> List.filter_map (fun (l : Odoc_unit.any) ->
           match l.kind with
           | `Intf { hidden = true; _ } -> None
           | _ when l.to_output -> Some l.odocl_file
           | _ -> None)
    |> List.sort_uniq Fpath.compare
  in
  let file_list = Fpath.(parent index.index_file / "index-inputs.txt") in
  Util.with_out_to file_list (fun oc ->
      List.iter (fun f -> Printf.fprintf oc "%s\n" (Fpath.to_string f)) inputs)
  |> Result.get_ok;
  file_list

(* The files every page relies on: odoc's support files and sherlodoc's
   runtime. *)
let html_support html_dir =
  let _ = OS.Dir.create html_dir |> Result.get_ok in
  Sherlodoc.js Fpath.(html_dir // Sherlodoc.js_file);
  ignore (Odoc.support_files html_dir)

let with_remaps remaps f =
  match remaps with
  | [] -> f None
  | remaps ->
      OS.File.with_tmp_oc "remap.%s.txt"
        (fun fpath oc () ->
          List.iter (fun (a, b) -> Printf.fprintf oc "%s:%s\n%!" a b) remaps;
          f (Some fpath))
        ()
      |> Result.get_ok

let generate ?remap_file ~generate_json html_dir (pkg : Odoc_unit.pkg) =
  (* The package's index, sidebar and search database, which its pages then
     point at. *)
  let search_uris, sidebar =
    match pkg.index with
    | None -> (None, None)
    | Some ({ index_file; sidebar_file; html_dir = pkg_html } as index) ->
        let file_list = index_file_list pkg index in
        Odoc.compile_index ~json:false ~output_file:index_file ~file_list
          ~simplified:false ~wrap:false ();
        Odoc.sidebar_generate ~output_file:sidebar_file ~json:false index_file
          ();
        Odoc.sidebar_generate
          ~output_file:Fpath.(html_dir // pkg_html / "sidebar.json")
          ~json:true index_file ();
        let db = Sherlodoc.db_js_file pkg_html in
        let _ = OS.Dir.create Fpath.(html_dir // pkg_html) |> Result.get_ok in
        Sherlodoc.index ~format:`js ~inputs:[ index_file ]
          ~dst:Fpath.(html_dir // db)
          ();
        Atomic.incr Stats.stats.generated_indexes;
        (Some [ db; Sherlodoc.js_file ], Some sidebar_file)
  in
  let output_dir = Fpath.to_string html_dir in
  let home_breadcrumb = "Package index" in
  let generate (l : Odoc_unit.any) =
    if l.to_output then
      let input_file = l.odocl_file in
      match l.kind with
      | `Intf { hidden = true; _ } -> ()
      | `Impl { src_path; _ } ->
          Odoc.html_generate_source ?search_uris ?sidebar ~output_dir
            ~input_file ~home_breadcrumb ~source:src_path ();
          Atomic.incr Stats.stats.generated_units;
          if generate_json then (
            Odoc.html_generate_source ?search_uris ?sidebar ~output_dir
              ~input_file ~source:src_path ~as_json:true ~home_breadcrumb ();
            Atomic.incr Stats.stats.generated_units)
      | `Asset ->
          Odoc.html_generate_asset ~output_dir ~input_file:l.odoc_file
            ~asset_path:l.input_file ~home_breadcrumb ()
      | `Intf _ | `Mld | `Md ->
          Odoc.html_generate ?search_uris ?sidebar ?remap:remap_file ~output_dir
            ~input_file ~home_breadcrumb ();
          Atomic.incr Stats.stats.generated_units;
          if generate_json then (
            Odoc.html_generate ?search_uris ?sidebar ~output_dir ~input_file
              ~as_json:true ~home_breadcrumb ();
            Atomic.incr Stats.stats.generated_units)
  in
  Fiber.List.iter generate (Odoc_unit.all_units pkg)

(* The JSON search index of a package, for ocaml.org. It is the only consumer
   of the occurrence counts, which is why it is a separate, final step. *)
let json_index ~occurrence_file html_dir (pkg : Odoc_unit.pkg) =
  match pkg.index with
  | None -> ()
  | Some ({ html_dir = pkg_html; _ } as index) ->
      let file_list = index_file_list pkg index in
      Odoc.compile_index ~json:true ~occurrence_file
        ~output_file:Fpath.(html_dir // pkg_html / "index.js")
        ~simplified:true ~wrap:true ~file_list ()
