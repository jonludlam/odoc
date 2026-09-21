(* The link-time reference scope of a package: the page trees ([-P]) and the
   module trees ([-L]) that its units may refer to. Every unit of a package is
   linked with the same scope. Paths are absolute. *)
type scope = { pages : (string * Fpath.t) list; libs : (string * Fpath.t) list }

let pp_scope fmt x =
  let sfp_pp =
    Fmt.(
      list ~sep:comma (fun fmt (a, b) ->
          Format.fprintf fmt "(%s, %a)" a Fpath.pp b))
  in
  Format.fprintf fmt "@[<hov>pages: [%a]@;libs: [%a]@]" sfp_pp x.pages sfp_pp
    x.libs

type sidebar = { output_file : Fpath.t; json : bool; pkg_dir : Fpath.t }

type index = {
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
  sidebar : sidebar option;
}

let pp_index fmt x =
  Format.fprintf fmt "@[<hov>output_file: %a@;json: %b@;search_dir: %a@]"
    Fpath.pp x.output_file x.json Fpath.pp x.search_dir

type 'a t = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  input_copy : Fpath.t option;
      (* Used to stash cmtis from virtual libraries into the odoc dir for voodoo mode.
         See https://github.com/ocaml/odoc/pull/1309 *)
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}

type intf_extra = {
  hidden : bool;
  hash : string;
  deps : (string * Digest.t) list;
}

and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type all_kinds = [ impl | intf | mld | asset | md ]
type any = all_kinds t

let rec pp_kind : all_kinds Fmt.t =
 fun fmt x ->
  match x with
  | `Intf x -> Format.fprintf fmt "`Intf %a" pp_intf_extra x
  | `Impl x -> Format.fprintf fmt "`Impl %a" pp_impl_extra x
  | `Mld -> Format.fprintf fmt "`Mld"
  | `Md -> Format.fprintf fmt "`Md"
  | `Asset -> Format.fprintf fmt "`Asset"

and pp_intf_extra fmt x =
  Format.fprintf fmt "@[<hov>hidden: %b@;hash: %s@;deps: [%a]@]" x.hidden x.hash
    Fmt.Dump.(list (pair string string))
    x.deps

and pp_impl_extra fmt x =
  Format.fprintf fmt "@[<hov>src_id: %s@;src_path: %a@]"
    (Odoc.Id.to_string x.src_id)
    Fpath.pp x.src_path

and pp : all_kinds t Fmt.t =
 fun fmt x ->
  Format.fprintf fmt
    "@[<hov>parent_id: %s@;\
     input_file: %a@;\
     odoc_file: %a@;\
     odocl_file: %a@;\
     kind:%a@;\
     @]"
    (Odoc.Id.to_string x.parent_id)
    Fpath.pp x.input_file Fpath.pp x.odoc_file Fpath.pp x.odocl_file pp_kind
    (x.kind :> all_kinds)

(* A library: its modules and their implementations, and the [-I] search path
   they are compiled and linked with. *)
type lib = { lib_name : string; includes : Fpath.t list; units : any list }

let pp_lib fmt x =
  Format.fprintf fmt "@[<hov>lib_name: %s@;includes: %a@;units: %a@]" x.lib_name
    Fmt.Dump.(list Fpath.pp)
    x.includes
    Fmt.Dump.(list pp)
    x.units

(* A package, the unit of building: its libraries, its pages (including the
   generated landing pages), the reference scope they all link with, and the
   index they are gathered in. *)
type pkg = {
  pkgname : string option;
  scope : scope;
  index : index option;
  libs : lib list;
  pages : any list;
}

let all_units pkg = pkg.pages @ List.concat_map (fun l -> l.units) pkg.libs

let pp_pkg fmt x =
  Format.fprintf fmt
    "@[<hov>pkgname: %a@;scope: %a@;index: %a@;libs: %a@;pages: %a@]"
    (Fmt.option Fmt.string) x.pkgname pp_scope x.scope (Fmt.option pp_index)
    x.index
    Fmt.Dump.(list pp_lib)
    x.libs
    Fmt.Dump.(list pp)
    x.pages

let pkg_dir : Packages.t -> Fpath.t = fun pkg -> pkg.pkg_dir
let doc_dir : Packages.t -> Fpath.t = fun pkg -> pkg.doc_dir
let lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  match lib.id_override with
  | Some id -> Fpath.v id
  | None -> Fpath.(doc_dir pkg / lib.Packages.lib_name)
let lib_obj_dir (pkg : Packages.t) (lib : Packages.libty) =
  Fpath.(pkg_dir pkg // lib.Packages.rel_dir)
let src_dir pkg = Fpath.(doc_dir pkg / "src")
let src_lib_dir (pkg : Packages.t) (lib : Packages.libty) =
  match lib.id_override with
  | Some id -> Fpath.v id
  | None -> Fpath.(src_dir pkg / lib.Packages.lib_name)

let output_root (u : _ t) =
  let dir = Fpath.parent u.odoc_file in
  match Odoc.Id.to_string u.parent_id with
  | "" -> dir
  | id ->
      let rec up d = function 0 -> d | n -> up (Fpath.parent d) (n - 1) in
      up dir (List.length (String.split_on_char '/' id))

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
let fix_virtual ~(precompiled_units : intf t list Util.StringMap.t)
    ~(units : intf t list Util.StringMap.t) =
  Logs.debug (fun m ->
      m "Fixing virtual libraries: %d precompiled units, %d other units"
        (Util.StringMap.cardinal precompiled_units)
        (Util.StringMap.cardinal units));
  let all =
    Util.StringMap.union
      (fun h x y ->
        Logs.debug (fun m ->
            m "Unifying hash %s (%d, %d)" h (List.length x) (List.length y));
        Some (x @ y))
      precompiled_units units
  in
  Util.StringMap.map
    (fun units ->
      List.map
        (fun unit ->
          let uhash = match unit.kind with `Intf { hash; _ } -> hash in
          if not (Fpath.has_ext "cmt" unit.input_file) then unit
          else
            match Util.StringMap.find uhash all with
            | [ _ ] -> unit
            | xs -> (
                let unit_name =
                  Fpath.rem_ext unit.input_file |> Fpath.basename
                in
                match
                  List.filter
                    (fun (x : intf t) ->
                      (match x.kind with `Intf { hash; _ } -> uhash = hash)
                      && Fpath.has_ext "cmti" x.input_file
                      && Fpath.rem_ext x.input_file |> Fpath.basename
                         = unit_name)
                    xs
                with
                | [ x ] -> { unit with input_file = x.input_file }
                | xs -> (
                    Logs.debug (fun m ->
                        m
                          "Duplicate hash found, but multiple (%d) matching \
                           cmti found for %a"
                          (List.length xs) Fpath.pp unit.input_file);
                    let possibles =
                      List.find_map
                        (fun x ->
                          match x.input_copy with
                          | Some x ->
                              if
                                x |> Bos.OS.File.exists
                                |> Result.value ~default:false
                              then Some x
                              else None
                          | None -> None)
                        xs
                    in
                    match possibles with
                    | None ->
                        Logs.debug (fun m -> m "Not replacing input file");
                        unit
                    | Some x ->
                        Logs.debug (fun m ->
                            m "Replacing input_file of unit with %a" Fpath.pp x);
                        { unit with input_file = x })))
        units)
    units
