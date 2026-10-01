type step =
  | Compile_lib of Odoc_unit.pkg * Odoc_unit.lib
  | Compile_pages of Odoc_unit.pkg
  | Link of Odoc_unit.pkg

(* The nameless package is the top-level index, of which there is one. *)
module NameSet = Set.Make (struct
  type t = string option

  let compare = compare
end)

type state = {
  libs_done : Util.StringSet.t;
  pkgs_done : NameSet.t;
  rev_steps : step list;
}

let of_several pkgs =
  let libs =
    List.fold_left
      (fun acc (p : Odoc_unit.pkg) ->
        List.fold_left
          (fun acc (lib : Odoc_unit.lib) ->
            Util.StringMap.add lib.lib_name (p, lib) acc)
          acc p.libs)
      Util.StringMap.empty pkgs
  in
  let by_name =
    List.fold_left
      (fun acc (p : Odoc_unit.pkg) ->
        match p.pkgname with
        | Some name -> Util.StringMap.add name p acc
        | None -> acc)
      Util.StringMap.empty pkgs
  in
  let rec compile_lib st (pkg, (lib : Odoc_unit.lib)) =
    if Util.StringSet.mem lib.lib_name st.libs_done then st
    else
      let st =
        { st with libs_done = Util.StringSet.add lib.lib_name st.libs_done }
      in
      let st =
        List.fold_left
          (fun st name ->
            match Util.StringMap.find_opt name libs with
            | Some required -> compile_lib st required
            | None -> st)
          st lib.requires
      in
      { st with rev_steps = Compile_lib (pkg, lib) :: st.rev_steps }
  in
  let compile_pkg st (pkg : Odoc_unit.pkg) =
    if NameSet.mem pkg.pkgname st.pkgs_done then st
    else
      let st = { st with pkgs_done = NameSet.add pkg.pkgname st.pkgs_done } in
      let st =
        List.fold_left (fun st lib -> compile_lib st (pkg, lib)) st pkg.libs
      in
      { st with rev_steps = Compile_pages pkg :: st.rev_steps }
  in
  let link st (pkg : Odoc_unit.pkg) =
    let st = compile_pkg st pkg in
    let st =
      List.fold_left
        (fun st (name, _) ->
          match Util.StringMap.find_opt name by_name with
          | Some in_scope -> compile_pkg st in_scope
          | None -> st)
        st pkg.scope.page_roots
    in
    { st with rev_steps = Link pkg :: st.rev_steps }
  in
  let st =
    List.fold_left link
      {
        libs_done = Util.StringSet.empty;
        pkgs_done = NameSet.empty;
        rev_steps = [];
      }
      pkgs
  in
  List.rev st.rev_steps

let of_packages pkgs = of_several pkgs

let compile_only pkg =
  List.filter (function Link _ -> false | _ -> true) (of_several [ pkg ])
