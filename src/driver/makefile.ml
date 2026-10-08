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

(* A rule for a command that writes one file. *)
let action_rule ~prereqs (a : Cmd_outputs.action) =
  {
    target = Option.get a.output;
    prereqs;
    actions = [ Action a ];
    stamp = false;
  }

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

(* A page, which needs no search path. *)
let page_rule (p : page) =
  {
    target = p.odoc_file;
    prereqs = [ p.input_file ];
    actions = [ Action (Commands.compile_page p) ];
    stamp = false;
  }

(* Compiling the modules of a library. A module waits for the modules of this
   library whose digest it imports, and for the libraries this one requires.
   An import of another library's module is covered by that library's stamp. *)
let lib_rules ~stamp_dir (lib : lib) =
  let by_hash = intfs_by_hash lib in
  let required = List.map (lib_stamp ~stamp_dir) lib.requires in
  let unit_rule (u : module_unit) =
    let prereqs =
      match u.kind with
      | `Intf { deps; _ } ->
          List.filter_map
            (fun (_, digest) ->
              Option.map
                (fun (d : module_unit) -> d.odoc_file)
                (Util.StringMap.find_opt digest by_hash))
            deps
          |> List.filter (fun f -> not (Fpath.equal f u.odoc_file))
      | `Impl _ ->
          (* An implementation is compiled against the same libraries as the
             interfaces, so it waits for all of them rather than for a digest
             it does not record. *)
          intf_odocs lib
    in
    {
      target = u.odoc_file;
      prereqs = (u.input_file :: prereqs) @ required;
      actions = List.map (fun a -> Action a) (Commands.compile_module lib u);
      stamp = false;
    }
  in
  let units = List.map unit_rule lib.units in
  let page_rules =
    List.map (fun p -> page_rule (p :> page)) (Option.to_list lib.page)
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
let link_rules ~stamp_dir ~warnings_tags ~scope_pkgs (pkg : pkg) =
  let scope_stamps = List.map (pkg_stamp ~stamp_dir) (pkg :: scope_pkgs pkg) in
  let link ~libs (u : any) =
    if not (is_output u) then None
    else
      Some
        {
          target = u.odocl_file;
          prereqs = u.odoc_file :: scope_stamps;
          actions = [ Action (Commands.link ~warnings_tags ~libs pkg u) ];
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
  let compile_index, from_index = Commands.index ~html_dir ~file_list index in
  {
    target = file_list;
    prereqs = inputs;
    actions = [ write_list ];
    stamp = false;
  }
  :: action_rule ~prereqs:(file_list :: inputs) compile_index
  :: List.map (action_rule ~prereqs:[ index.index_file ]) from_index

let generate_rules ~html_dir ~stamp_dir ?remap_file ~generate_json (pkg : pkg) =
  let sidebar =
    Option.map (fun (index : index) -> index.sidebar_file) pkg.index
  in
  let rule (u : any) =
    match Commands.generate ~html_dir ?remap_file ~generate_json pkg u with
    | [] -> None
    | actions ->
        let input =
          match u.kind with `Asset -> u.odoc_file | _ -> u.odocl_file
        in
        Some
          {
            target = html_stamp ~stamp_dir u;
            prereqs =
              (input :: Option.to_list sidebar) @ Option.to_list remap_file;
            actions = List.map (fun a -> Action a) actions;
            stamp = true;
          }
  in
  List.filter_map rule (all_units pkg)

let emit ppf ~html_dir ~stamp_dir ~remaps ~generate_json ~warnings_tags pkgs =
  let scope_pkgs = scope_pkgs pkgs in
  (* The remap file outlives the driver, which a temporary file would not, so
     it goes beside the stamps. *)
  let remap_file =
    match remaps with
    | [] -> None
    | remaps ->
        let file = Fpath.(stamp_dir / "remap.txt") in
        Util.with_out_to file (fun oc ->
            List.iter (fun (a, b) -> Printf.fprintf oc "%s:%s\n" a b) remaps)
        |> Result.get_ok;
        Some file
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
        @ link_rules ~stamp_dir ~warnings_tags ~scope_pkgs pkg
        @ (match pkg.index with
          | None -> []
          | Some index -> index_rules ~html_dir pkg index)
        @ generate_rules ~html_dir ~stamp_dir ?remap_file ~generate_json pkg)
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
