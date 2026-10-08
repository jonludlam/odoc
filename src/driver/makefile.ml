open Odoc_unit

(* A command, its target and what it waits for. *)
type recipe =
  | Action of Cmd_outputs.action  (** A command the driver would run. *)
  | Shell of string  (** A line for the shell, written as it is. *)

type rule = {
  target : Fpath.t;
  prereqs : Fpath.t list;
  actions : recipe list;
  stamp : bool;  (** The target is a stamp file, touched by the recipe. *)
}

(* A path below the stamp directory that stands for an absolute one. *)
let under dir path =
  let parts = Fpath.segs path |> List.filter (fun s -> s <> "" && s <> ".") in
  Fpath.(dir // v (String.concat "/" parts))

(* make treats a [$] as the start of a variable. Nothing else in a path or a
   command needs escaping, since each word is quoted. *)
let escape s = String.concat "$$" (String.split_on_char '$' s)

let quote_cmd cmd =
  Bos.Cmd.to_list cmd |> List.map Filename.quote |> String.concat " " |> escape

let pp_rule ppf { target; prereqs; actions; stamp } =
  let prereqs = List.sort_uniq Fpath.compare prereqs in
  Format.fprintf ppf "@[<h>%s:" (escape (Fpath.to_string target));
  List.iter
    (fun p -> Format.fprintf ppf " %s" (escape (Fpath.to_string p)))
    prereqs;
  Format.fprintf ppf "@]@\n";
  Format.fprintf ppf "\t@@mkdir -p $(@@D)@\n";
  List.iter
    (function
      | Action (a : Cmd_outputs.action) ->
          Format.fprintf ppf "\t%s@\n" (quote_cmd a.cmd)
      | Shell line -> Format.fprintf ppf "\t%s@\n" (escape line))
    actions;
  if stamp then Format.fprintf ppf "\t@@touch $@@@\n";
  Format.fprintf ppf "@\n"

(* The stamp that stands for a set of files. *)
let lib_stamp ~stamp_dir (lib : lib) = Fpath.(stamp_dir / "lib" / lib.lib_name)

let pkg_stamp ~stamp_dir (pkg : pkg) =
  Fpath.(stamp_dir / "pkg" / Option.value ~default:"_index" pkg.pkgname)

let html_stamp ~stamp_dir (u : any) =
  under Fpath.(stamp_dir / "html") u.odocl_file

(* The units of a library, by the digest of their interface: an import names
   a digest, and the unit that answers it must be compiled first. *)
let intfs_by_hash (lib : lib) =
  List.fold_left
    (fun acc (u : module_unit) ->
      match u.kind with
      | `Intf { hash; _ } -> Util.StringMap.add hash u acc
      | `Impl _ -> acc)
    Util.StringMap.empty lib.units

let intf_odocs (lib : lib) =
  List.filter_map
    (fun (u : module_unit) ->
      match u.kind with `Intf _ -> Some u.odoc_file | `Impl _ -> None)
    lib.units

(* Compiling the modules of a library. A module waits for the modules of this
   library whose digest it imports, and for the libraries this one requires.
   An import of another library's module is covered by that library's stamp. *)
let lib_rules ~stamp_dir (lib : lib) =
  let by_hash = intfs_by_hash lib in
  let required = List.map (lib_stamp ~stamp_dir) lib.requires in
  let unit_rule (u : module_unit) =
    match u.kind with
    | `Intf { hidden = _; hash = _; deps } ->
        let imported =
          List.filter_map
            (fun (_, digest) ->
              Option.map
                (fun (d : module_unit) -> d.odoc_file)
                (Util.StringMap.find_opt digest by_hash))
            deps
          |> List.filter (fun f -> not (Fpath.equal f u.odoc_file))
        in
        let copy =
          match u.input_copy with
          | None -> []
          | Some dst ->
              [
                Action
                  {
                    Cmd_outputs.log = None;
                    desc = "Copying the interface";
                    cmd =
                      Bos.Cmd.(
                        v "cp"
                        % Fpath.to_string u.input_file
                        % Fpath.to_string dst);
                    output = Some dst;
                    ignore_failures = false;
                  };
              ]
        in
        Some
          {
            target = u.odoc_file;
            prereqs = (u.input_file :: imported) @ required;
            actions =
              Action
                (Odoc.compile ~output_file:u.odoc_file ~input_file:u.input_file
                   ~libs:lib.includes ~warnings_tag:lib.pkgname
                   ~parent_id:u.parent_id ~ignore_output:(not u.enable_warnings))
              :: copy;
            stamp = false;
          }
    | `Impl { src_id; _ } ->
        (* An implementation is compiled against the same libraries as the
           interfaces, so it waits for all of them rather than for a digest
           it does not record. *)
        Some
          {
            target = u.odoc_file;
            prereqs = (u.input_file :: intf_odocs lib) @ required;
            actions =
              [
                Action
                  (Odoc.compile_impl ~output_file:u.odoc_file
                     ~input_file:u.input_file ~libs:lib.includes
                     ~parent_id:u.parent_id ~source_id:src_id);
              ];
            stamp = false;
          }
  in
  let units = List.filter_map unit_rule lib.units in
  let page_rules =
    List.map
      (fun (p : mld t) ->
        {
          target = p.odoc_file;
          prereqs = [ p.input_file ];
          actions =
            [
              Action
                (Odoc.compile ~output_file:p.odoc_file ~input_file:p.input_file
                   ~libs:[] ~warnings_tag:None ~parent_id:p.parent_id
                   ~ignore_output:(not p.enable_warnings));
            ];
          stamp = false;
        })
      (Option.to_list lib.page)
  in
  let stamp =
    {
      target = lib_stamp ~stamp_dir lib;
      prereqs =
        List.map (fun r -> r.target) units
        @ List.map (fun r -> r.target) page_rules;
      actions = [];
      stamp = true;
    }
  in
  units @ page_rules @ [ stamp ]

(* The pages of a package, which need no search path. *)
let page_rule (p : page) =
  let actions =
    match p.kind with
    | `Mld ->
        [
          Action
            (Odoc.compile ~output_file:p.odoc_file ~input_file:p.input_file
               ~libs:[] ~warnings_tag:None ~parent_id:p.parent_id
               ~ignore_output:(not p.enable_warnings));
        ]
    | `Md ->
        [
          Action
            (Odoc.compile_md ~output_file:p.odoc_file ~input_file:p.input_file
               ~parent_id:p.parent_id);
        ]
    | `Asset ->
        [
          Action
            (Odoc.compile_asset ~output_file:p.odoc_file ~parent_id:p.parent_id
               ~name:(Fpath.filename p.input_file));
        ]
  in
  { target = p.odoc_file; prereqs = [ p.input_file ]; actions; stamp = false }

let compile_rules ~stamp_dir (pkg : pkg) =
  let libs = List.concat_map (lib_rules ~stamp_dir) pkg.libs in
  let pages = List.map page_rule pkg.pages in
  let stamp =
    {
      target = pkg_stamp ~stamp_dir pkg;
      prereqs =
        List.map (lib_stamp ~stamp_dir) pkg.libs
        @ List.map (fun r -> r.target) pages;
      actions = [];
      stamp = true;
    }
  in
  libs @ pages @ [ stamp ]

(* Linking waits for the package itself and for every package its scope
   names, since a reference may lead into any of them. *)
let link_rules ~stamp_dir ~warnings_tags ~by_name (pkg : pkg) =
  let scope_stamps =
    pkg_stamp ~stamp_dir pkg
    :: List.filter_map
         (fun (name, _) ->
           Option.map (pkg_stamp ~stamp_dir)
             (Util.StringMap.find_opt name by_name))
         pkg.scope.page_roots
  in
  let link ~libs (u : any) =
    if not (is_output u) then None
    else
      Some
        {
          target = u.odocl_file;
          prereqs = u.odoc_file :: scope_stamps;
          actions =
            [
              Action
                (Odoc.link ~input_file:u.odoc_file ~output_file:u.odocl_file
                   ~libs ~docs:pkg.scope.page_roots
                   ~ignore_output:(not u.enable_warnings) ~warnings_tags
                   ?current_package:pkg.pkgname ());
            ];
          stamp = false;
        }
  in
  List.filter_map (fun (libs, u) -> link ~libs u) (link_units pkg)

(* The units a package's index is built from: the same list the driver
   writes, and a rule that writes it. *)
let index_rules ~html_dir (pkg : pkg) (index : index) =
  let inputs = index_inputs pkg in
  let file_list = Fpath.(parent index.index_file / "index-inputs.txt") in
  (* The list of files to index is written by the recipe, so that it is
     rebuilt with the rest. A shell line, since it redirects. *)
  let write_list =
    Shell
      (Printf.sprintf "printf '%%s\\n' %s > %s"
         (String.concat " "
            (List.map (fun f -> Filename.quote (Fpath.to_string f)) inputs))
         (Filename.quote (Fpath.to_string file_list)))
  in
  let json_sidebar = Fpath.(html_dir // index.html_dir / "sidebar.json") in
  let db = Fpath.(html_dir // Sherlodoc.db_js_file index.html_dir) in
  [
    {
      target = file_list;
      prereqs = inputs;
      actions = [ write_list ];
      stamp = false;
    };
    {
      target = index.index_file;
      prereqs = file_list :: inputs;
      actions =
        [
          Action
            (Odoc.compile_index ~json:false ~output_file:index.index_file
               ~file_list ~simplified:false ~wrap:false ());
        ];
      stamp = false;
    };
    {
      target = index.sidebar_file;
      prereqs = [ index.index_file ];
      actions =
        [
          Action
            (Odoc.sidebar_generate ~output_file:index.sidebar_file ~json:false
               index.index_file ());
        ];
      stamp = false;
    };
    {
      target = json_sidebar;
      prereqs = [ index.index_file ];
      actions =
        [
          Action
            (Odoc.sidebar_generate ~output_file:json_sidebar ~json:true
               index.index_file ());
        ];
      stamp = false;
    };
    {
      target = db;
      prereqs = [ index.index_file ];
      actions =
        [
          Action
            (Sherlodoc.index ~format:`js ~inputs:[ index.index_file ] ~dst:db ());
        ];
      stamp = false;
    };
  ]

let generate_rules ~html_dir ~stamp_dir (pkg : pkg) =
  let output_dir = Fpath.to_string html_dir in
  let home_breadcrumb = "Package index" in
  let sidebar, search_uris =
    match pkg.index with
    | None -> (None, None)
    | Some index ->
        ( Some index.sidebar_file,
          Some [ Sherlodoc.db_js_file index.html_dir; Sherlodoc.js_file ] )
  in
  let rule (u : any) =
    if not (is_output u) then None
    else
      let actions =
        match u.kind with
        | `Impl { src_path; _ } ->
            [
              Action
                (Odoc.html_generate_source ~output_dir ?sidebar ?search_uris
                   ~input_file:u.odocl_file ~source:src_path ~home_breadcrumb ());
            ]
        | `Asset ->
            [
              Action
                (Odoc.html_generate_asset ~output_dir ~input_file:u.odoc_file
                   ~asset_path:u.input_file ~home_breadcrumb ());
            ]
        | `Intf _ | `Mld | `Md ->
            [
              Action
                (Odoc.html_generate ~output_dir ?sidebar ?search_uris
                   ~input_file:u.odocl_file ~home_breadcrumb ());
            ]
      in
      let input =
        match u.kind with `Asset -> u.odoc_file | _ -> u.odocl_file
      in
      Some
        {
          target = html_stamp ~stamp_dir u;
          prereqs = input :: Option.to_list sidebar;
          actions;
          stamp = true;
        }
  in
  List.filter_map rule (all_units pkg)

let emit ppf ~html_dir ~stamp_dir ~warnings_tags pkgs =
  let by_name =
    List.fold_left
      (fun acc (p : pkg) ->
        match p.pkgname with
        | Some name -> Util.StringMap.add name p acc
        | None -> acc)
      Util.StringMap.empty pkgs
  in
  let support =
    {
      target = Fpath.(stamp_dir / "support-files");
      prereqs = [];
      actions = [ Action (Odoc.support_files html_dir) ];
      stamp = true;
    }
  in
  let sherlodoc_js =
    {
      target = Fpath.(html_dir // Sherlodoc.js_file);
      prereqs = [];
      actions = [ Action (Sherlodoc.js Fpath.(html_dir // Sherlodoc.js_file)) ];
      stamp = false;
    }
  in
  let rules =
    List.concat_map
      (fun pkg ->
        compile_rules ~stamp_dir pkg
        @ link_rules ~stamp_dir ~warnings_tags ~by_name pkg
        @ (match pkg.index with
          | None -> []
          | Some index -> index_rules ~html_dir pkg index)
        @ generate_rules ~html_dir ~stamp_dir pkg)
      pkgs
    @ [ support; sherlodoc_js ]
  in
  (* What nothing else waits for: the end of each chain. *)
  let final =
    let needed =
      List.fold_left
        (fun acc r ->
          List.fold_left (fun acc p -> Fpath.Set.add p acc) acc r.prereqs)
        Fpath.Set.empty rules
    in
    List.filter_map
      (fun r -> if Fpath.Set.mem r.target needed then None else Some r.target)
      rules
  in
  Format.fprintf ppf
    "# Written by odoc_driver. It builds the same documentation as a run of@\n\
     # the driver. Run it with -j for the parallelism the driver has.@\n\
     @\n";
  Format.fprintf ppf "@[<h>all:";
  List.iter
    (fun t -> Format.fprintf ppf " %s" (escape (Fpath.to_string t)))
    (List.sort_uniq Fpath.compare final);
  Format.fprintf ppf "@]@\n@\n.PHONY: all@\n@\n";
  List.iter (pp_rule ppf) rules
