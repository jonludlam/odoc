type t = {
  pkgs : Odoc_unit.pkg list;
  by_name : Odoc_unit.pkg Util.StringMap.t;
  libs : (Odoc_unit.pkg * Odoc_unit.lib) Util.StringMap.t;
}

let of_packages pkgs =
  let by_name =
    List.fold_left
      (fun acc (p : Odoc_unit.pkg) ->
        match p.pkgname with
        | Some name -> Util.StringMap.add name p acc
        | None -> acc)
      Util.StringMap.empty pkgs
  in
  let libs =
    List.fold_left
      (fun acc (p : Odoc_unit.pkg) ->
        List.fold_left
          (fun acc (lib : Odoc_unit.lib) ->
            Util.StringMap.add lib.lib_name (p, lib) acc)
          acc p.libs)
      Util.StringMap.empty pkgs
  in
  { pkgs; by_name; libs }

let packages t = t.pkgs

let requires t (lib : Odoc_unit.lib) =
  List.filter_map (fun name -> Util.StringMap.find_opt name t.libs) lib.requires

let scope_packages t (pkg : Odoc_unit.pkg) =
  List.filter_map
    (fun (name, _) -> Util.StringMap.find_opt name t.by_name)
    pkg.scope.page_roots
