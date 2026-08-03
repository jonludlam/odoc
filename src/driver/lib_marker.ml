open Sexplib.Std

let filename = ".odoc_lib_marker"

(* The wrapper the record is written inside, so that a marker of any other shape
   — including the prose marker written before this record existed — is
   recognisably not ours rather than mis-parsed. Bump [version] whenever the
   record below changes shape. *)
let tag = "odoc-lib-marker"
let version = 1

type module_info = {
  m_name : string;
  digest : string;
  stashed_cmti : string option;
}

and t = {
  lib_name : string;
  pkg_name : string;
  requires : string list;
  modules : module_info list;
}
[@@deriving sexp]

let of_lib ~pkg_name (lib : Packages.libty) =
  let module_info (m : Packages.modulety) =
    {
      m_name = m.m_name;
      digest = m.m_intf.mif_hash;
      (* Only virtual libraries stash their interface: see
         [Odoc_unit.stash_basename]. *)
      stashed_cmti =
        (match lib.archive_name with
        | Some _ -> None
        | None -> Some (Odoc_unit.stash_basename m.m_intf));
    }
  in
  {
    lib_name = lib.lib_name;
    pkg_name;
    requires = Util.StringSet.elements lib.lib_deps;
    modules = List.map module_info lib.modules;
  }

let to_string t =
  Sexplib.Sexp.to_string_hum
    (Sexplib.Sexp.List [ Atom tag; Atom (string_of_int version); sexp_of_t t ])
  ^ "\n"

let of_string s =
  try
    match Sexplib.Sexp.of_string s with
    | Sexplib0.Sexp.List [ Atom t; Atom v; body ]
      when t = tag && v = string_of_int version ->
        Some (t_of_sexp body)
    | _ -> None
  with _ -> None
