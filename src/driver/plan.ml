type t = Odoc_unit.pkg Util.StringMap.t

let of_packages pkgs =
  List.fold_left
    (fun acc (p : Odoc_unit.pkg) ->
      match p.pkgname with
      | Some name -> Util.StringMap.add name p acc
      | None -> acc)
    Util.StringMap.empty pkgs

let scope_packages t (pkg : Odoc_unit.pkg) =
  List.filter_map
    (fun (name, _) -> Util.StringMap.find_opt name t)
    pkg.scope.page_roots
