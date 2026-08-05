open Sexplib.Std

(* Each record is written inside a tagged, versioned wrapper, so that a file of
   any other shape — including the prose markers written before these records
   existed — is recognisably not ours rather than mis-parsed. Bump [version]
   whenever either record changes shape. *)
let version = 1

let wrap tag sexp =
  Sexplib.Sexp.to_string_hum
    (Sexplib.Sexp.List [ Atom tag; Atom (string_of_int version); sexp ])
  ^ "\n"

let unwrap tag parse s =
  try
    match Sexplib.Sexp.of_string s with
    | Sexplib0.Sexp.List [ Atom t; Atom v; body ]
      when t = tag && v = string_of_int version ->
        Some (parse body)
    | _ -> None
  with _ -> None

module Pkg = struct
  let filename = ".odoc_pkg_marker"
  let tag = "odoc-pkg-marker"

  type t = { pkg_name : string; libraries : (string * string list) list }
  [@@deriving sexp]

  let to_string t = wrap tag (sexp_of_t t)
  let of_string s = unwrap tag t_of_sexp s
end

module Lib = struct
  let filename = ".odoc_lib_marker"
  let tag = "odoc-lib-marker"

  type module_info = {
    m_name : string;
    digest : string;
    stashed_cmti : string option;
  }

  and t = { lib_name : string; pkg_name : string; modules : module_info list }
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
      modules = List.map module_info lib.modules;
    }

  let to_string t = wrap tag (sexp_of_t t)
  let of_string s = unwrap tag t_of_sexp s
end
