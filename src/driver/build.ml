(* Build packages one at a time.

   Libraries are compiled in dependency order the way [Compile.compile_lib]
   orders modules: [compile_lib] compiles a library after the libraries it
   requires, memoised by name. The recursion is over libraries rather than
   packages because META requires form a DAG between libraries even where
   packages depend on each other (ppx_deriving.api needs ppxlib,
   ppxlib.traverse needs ppx_deriving.runtime).

   Linking a package needs every package in its reference scope, and
   odoc-config.sexp can put a package built later there -- eio points at
   eio_main, odoc at odoc-driver. So before a package is linked, whatever its
   scope names is compiled too. This is the split that voodoo mode exposes as
   [--actions compile-only] / [--actions link-and-gen].

   The top-level index, whose scope is every package, is expected last. *)

let all ~html_dir ~remaps ~generate_json ~warnings_tags
    (pkgs : Odoc_unit.pkg list) =
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
  let compiled_libs = Hashtbl.create 1000 in
  let rec compile_lib name =
    if not (Hashtbl.mem compiled_libs name) then (
      Hashtbl.add compiled_libs name ();
      match Util.StringMap.find_opt name libs with
      | None -> ()
      | Some (pkg, lib) ->
          List.iter compile_lib lib.requires;
          Logs.debug (fun m -> m "Compiling library %s" name);
          Compile.compile_lib pkg lib)
  in
  let compiled_pkgs = Hashtbl.create 100 in
  let compile_pkg (pkg : Odoc_unit.pkg) =
    if not (Hashtbl.mem compiled_pkgs pkg.pkgname) then (
      Hashtbl.add compiled_pkgs pkg.pkgname ();
      List.iter (fun (lib : Odoc_unit.lib) -> compile_lib lib.lib_name) pkg.libs;
      Compile.compile_pages pkg)
  in
  Compile.html_support html_dir;
  Compile.with_remaps remaps @@ fun remap_file ->
  List.iter
    (fun (pkg : Odoc_unit.pkg) ->
      compile_pkg pkg;
      List.iter
        (fun (name, _) ->
          Option.iter compile_pkg (Util.StringMap.find_opt name by_name))
        pkg.scope.page_roots;
      Logs.debug (fun m -> m "Linking %a" (Fmt.option Fmt.string) pkg.pkgname);
      Compile.link ~warnings_tags pkg;
      Compile.generate ?remap_file ~generate_json html_dir pkg)
    pkgs
