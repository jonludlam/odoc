(* compile *)

open Bos

type compiled = Odoc_unit.pkg

let mk_byhash (units : Odoc_unit.any list) =
  List.fold_left
    (fun acc (u : Odoc_unit.any) ->
      match u.Odoc_unit.kind with
      | `Intf { hash; _ } as kind ->
          let elt = { u with kind } in
          Util.StringMap.update hash
            (function None -> Some [ elt ] | Some x -> Some (elt :: x))
            acc
      | _ -> acc)
    Util.StringMap.empty units

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

open Eio.Std

(* What a unit inherits from its library and package for the compile step. *)
type ctx = { includes : Fpath.Set.t; pkgname : string option }

(* Every unit of [pkgs], with its context, keyed by the unit's [odoc_file]
   (which is unique). Pages have no [-I]. *)
let contexts (pkgs : Odoc_unit.pkg list) : (Fpath.t, ctx) Hashtbl.t * _ list =
  let tbl = Hashtbl.create 1000 in
  let add includes (pkg : Odoc_unit.pkg) (u : Odoc_unit.any) =
    Hashtbl.replace tbl u.odoc_file { includes; pkgname = pkg.pkgname };
    u
  in
  let all =
    List.concat_map
      (fun (pkg : Odoc_unit.pkg) ->
        List.map (add Fpath.Set.empty pkg) pkg.pages
        @ List.concat_map
            (fun (lib : Odoc_unit.lib) ->
              List.map (add (Fpath.Set.of_list lib.includes) pkg) lib.units)
            pkg.libs)
      pkgs
  in
  (tbl, all)

(* An implementation of a virtual library ships its modules as [.cmt] files
   only: the interface, and the documentation written in it, belong to the
   virtual library's [.cmti], which shares the module's digest. When the
   virtual library is built in the same run, [Packages.remap_virtual] has
   already substituted the [.cmti]; when it was built earlier (voodoo mode),
   the copy stashed next to its [.odoc] file ([input_copy]) is on the
   implementation's dependency cone, so look for a same-named [.cmti] with the
   right digest along the unit's [-I] path. *)
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
    match List.find_map candidate (Fpath.Set.elements includes) with
    | None -> unit
    | Some cmti ->
        Logs.debug (fun m ->
            m "Using %a as the interface of %a" Fpath.pp cmti Fpath.pp
              unit.input_file);
        { unit with input_file = cmti }

let compile (pkgs : Odoc_unit.pkg list) =
  let ctx_tbl, all = contexts pkgs in
  let ctx (u : _ Odoc_unit.t) = Hashtbl.find ctx_tbl u.odoc_file in
  let hashes = mk_byhash all in
  let compile_mod =
    (* Modules have dependencies and must be compiled in the right order. Only
       the modules of [pkgs] are compiled here: a dependency whose digest is
       not among them belongs to a package built earlier, and its [.odoc] is
       found through the [-I] path. *)
    let tbl = Hashtbl.create 1000 in
    let compile_one compile_other (unit : Odoc_unit.intf Odoc_unit.t) =
      let (`Intf { Odoc_unit.deps; _ }) = unit.kind in
      let _fibers =
        Fiber.List.map
          (fun (other_unit_name, other_unit_hash) ->
            match compile_other other_unit_hash with
            | Ok r -> Some r
            | Error _exn ->
                Logs.debug (fun m ->
                    m
                      "Module %s (hash %s, required by %s) is not being \
                       compiled in this run"
                      other_unit_name other_unit_hash
                      (Fpath.filename unit.input_file));
                None)
          deps
      in
      let { includes; pkgname } = ctx unit in
      let unit = find_virtual_interface ~includes unit in
      Odoc.compile ~output_file:unit.odoc_file ~input_file:unit.input_file
        ~includes ~warnings_tag:pkgname ~parent_id:unit.parent_id
        ~ignore_output:(not unit.enable_warnings);
      (match unit.input_copy with
      | None -> ()
      | Some p -> Util.cp (Fpath.to_string unit.input_file) (Fpath.to_string p));
      Atomic.incr Stats.stats.compiled_units
    in
    let rec compile_mod : string -> ('a list, [> `Msg of string ]) Result.t =
     fun hash ->
      let map_units =
        Fiber.List.map (fun unit ->
            match
              Hashtbl.find_opt tbl
                (hash, Odoc.Id.to_string unit.Odoc_unit.parent_id)
            with
            | Some p ->
                Promise.await p;
                None
            | None ->
                let p, r = Promise.create () in
                Hashtbl.add tbl (hash, Odoc.Id.to_string unit.parent_id) p;
                let _result = compile_one compile_mod unit in
                Promise.resolve r ();
                Some unit)
      in
      try
        let units = Util.StringMap.find hash hashes in
        let r = map_units units in
        Ok (List.filter_map Fun.id r)
      with Not_found ->
        Error (`Msg ("Module with hash " ^ hash ^ " not found"))
    in
    compile_mod
  in

  let compile (unit : Odoc_unit.any) =
    match unit.kind with
    | `Intf intf -> (compile_mod intf.hash :> (Odoc_unit.any list, _) Result.t)
    | `Impl src ->
        let { includes; _ } = ctx unit in
        let source_id = src.src_id in
        Odoc.compile_impl ~output_file:unit.odoc_file
          ~input_file:unit.input_file ~includes ~parent_id:unit.parent_id
          ~source_id;
        Atomic.incr Stats.stats.compiled_impls;
        Ok [ unit ]
    | `Asset ->
        Odoc.compile_asset
          ~output_dir:(Odoc_unit.output_root unit)
          ~parent_id:unit.parent_id
          ~name:(Fpath.filename unit.input_file);
        Atomic.incr Stats.stats.compiled_assets;
        Ok [ unit ]
    | `Mld ->
        let includes = Fpath.Set.empty in
        Odoc.compile ~output_file:unit.odoc_file ~input_file:unit.input_file
          ~includes ~warnings_tag:None ~parent_id:unit.parent_id
          ~ignore_output:(not unit.enable_warnings);
        Atomic.incr Stats.stats.compiled_mlds;
        Ok [ unit ]
    | `Md ->
        Odoc.compile_md
          ~output_dir:(Odoc_unit.output_root unit)
          ~input_file:unit.input_file ~parent_id:unit.parent_id;
        Atomic.incr Stats.stats.compiled_mlds;
        Ok [ unit ]
  in
  let _ = Fiber.List.map compile all in
  pkgs

type linked = Odoc_unit.pkg

let link : warnings_tags:string list -> custom_layout:bool -> compiled list -> _
    =
 fun ~warnings_tags ~custom_layout pkgs ->
  let link (pkg : Odoc_unit.pkg) ~includes (c : Odoc_unit.any) =
    let link input_file output_file enable_warnings =
      let ({ libs; pages } : Odoc_unit.scope) = pkg.scope in
      Odoc.link ~custom_layout ~input_file ~output_file ~libs ~docs:pages
        ~includes ~ignore_output:(not enable_warnings) ~warnings_tags
        ?current_package:pkg.pkgname ()
    in
    match c.kind with
    | `Intf { hidden = true; _ } ->
        Logs.debug (fun m -> m "not linking %a" Fpath.pp c.odoc_file)
    | _ -> (
        Logs.debug (fun m -> m "linking %a" Fpath.pp c.odoc_file);
        if c.to_output then link c.odoc_file c.odocl_file c.enable_warnings;
        match c.kind with
        | `Intf _ -> Atomic.incr Stats.stats.linked_units
        | `Mld -> Atomic.incr Stats.stats.linked_mlds
        | `Asset -> ()
        | `Impl _ -> Atomic.incr Stats.stats.linked_impls
        | `Md -> Atomic.incr Stats.stats.linked_mlds)
  in
  let jobs =
    List.concat_map
      (fun (pkg : Odoc_unit.pkg) ->
        List.map (fun u -> (pkg, [], u)) pkg.pages
        @ List.concat_map
            (fun (lib : Odoc_unit.lib) ->
              List.map (fun u -> (pkg, lib.includes, u)) lib.units)
            pkg.libs)
      pkgs
  in
  Fiber.List.iter (fun (pkg, includes, u) -> link pkg ~includes u) jobs;
  pkgs

let sherlodoc_index_one ~output_dir (index : Odoc_unit.index) =
  let inputs = [ index.output_file ] in
  let rel_path = Fpath.(index.search_dir / "sherlodoc_db.js") in
  let dst = Fpath.(output_dir // rel_path) in
  let dst_dir, _ = Fpath.split_base dst in
  let _ = OS.Dir.create dst_dir |> Result.get_ok in
  Sherlodoc.index ~format:`js ~inputs ~dst ();
  rel_path

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
  let file_list = Fpath.(parent index.output_file / "index-inputs.txt") in
  Util.with_out_to file_list (fun oc ->
      List.iter (fun f -> Printf.fprintf oc "%s\n" (Fpath.to_string f)) inputs)
  |> Result.get_ok;
  file_list

let html_generate ~remaps ~generate_json output_dir (pkgs : linked list) =
  let _ = OS.Dir.create output_dir |> Result.get_ok in
  Sherlodoc.js Fpath.(output_dir // Sherlodoc.js_file);
  let compile_index (pkg : Odoc_unit.pkg)
      ({ output_file; json; search_dir = _; sidebar } as index :
        Odoc_unit.index) =
    let file_list = index_file_list pkg index in
    let () =
      Odoc.compile_index ~json ~output_file ~file_list ~simplified:false
        ~wrap:false ()
    in
    let sidebar =
      match sidebar with
      | None -> None
      | Some { output_file; json; pkg_dir } ->
          Odoc.sidebar_generate ~output_file ~json index.output_file ();
          Odoc.sidebar_generate
            ~output_file:Fpath.(output_dir // pkg_dir / "sidebar.json")
            ~json:true index.output_file ();
          Some output_file
    in
    let db_path = sherlodoc_index_one ~output_dir index in
    Atomic.incr Stats.stats.generated_indexes;
    let search_uris = [ db_path; Sherlodoc.js_file ] in
    (Some search_uris, sidebar)
  in
  let html_generate ~remap_file ~search_uris ~sidebar (l : Odoc_unit.any) =
    if l.to_output then
      let output_dir = Fpath.to_string output_dir in
      let home_breadcrumb = "Package index" in
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
      | _ ->
          Odoc.html_generate ?search_uris ?sidebar ?remap:remap_file ~output_dir
            ~input_file ~home_breadcrumb ();
          Atomic.incr Stats.stats.generated_units;
          if generate_json then (
            Odoc.html_generate ?search_uris ?sidebar ~output_dir ~input_file
              ~as_json:true ~home_breadcrumb ();
            Atomic.incr Stats.stats.generated_units)
  in
  let generate_pkg remap_file (pkg : Odoc_unit.pkg) =
    let search_uris, sidebar =
      match pkg.index with
      | None -> (None, None)
      | Some index -> compile_index pkg index
    in
    Fiber.List.iter
      (html_generate ~remap_file ~search_uris ~sidebar)
      (Odoc_unit.all_units pkg)
  in
  if List.length remaps = 0 then Fiber.List.iter (generate_pkg None) pkgs
  else
    Bos.OS.File.with_tmp_oc "remap.%s.txt"
      (fun fpath oc () ->
        List.iter (fun (a, b) -> Printf.fprintf oc "%s:%s\n%!" a b) remaps;
        Fiber.List.iter (generate_pkg (Some fpath)) pkgs)
      ()
    |> ignore

(* The JSON search index of a package, for ocaml.org. It is the only consumer
   of the occurrence counts, which is why it is a separate, final step. *)
let json_index ~occurrence_file output_dir (pkgs : linked list) =
  Fiber.List.iter
    (fun (pkg : Odoc_unit.pkg) ->
      match pkg.index with
      | Some ({ sidebar = Some { pkg_dir; _ }; _ } as index) ->
          let file_list = index_file_list pkg index in
          Odoc.compile_index ~json:true ~occurrence_file
            ~output_file:Fpath.(output_dir // pkg_dir / "index.js")
            ~simplified:true ~wrap:true ~file_list ()
      | _ -> ())
    pkgs
