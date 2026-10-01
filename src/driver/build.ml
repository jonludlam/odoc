(* Build packages one at a time.

   Libraries are compiled in dependency order, the same way
   [Compile.compile_lib] orders modules: [compile_lib] compiles a library
   after the libraries it requires, and remembers which libraries it has done.
   The recursion is over libraries because requiring is a relation between
   libraries; a package is only a grouping of them. No library requires
   itself, directly or not, since the compiler could not have built it, so
   the recursion always ends.

   Linking a package needs every package in its reference scope, and
   odoc-config.sexp can put a package built later there: eio's documentation
   points at eio_main, odoc's at odoc-driver. So before a package is linked,
   whatever its scope names is compiled too. This is the split that voodoo
   mode exposes as [--actions compile-only] and [--actions link-and-gen].

   A {!Plan} resolves the names a scope and a library's requires hold. The
   top-level index, whose scope is every package, is expected last. *)

(* Compiling a package's libraries, each after the libraries it requires and
   each at most once. The plan holds the libraries of every package in the
   run, so that a library pulled in by another package's scope is compiled
   once. *)
let compile_libs_of plan =
  let compiled_libs = Hashtbl.create 1000 in
  let rec compile_lib (pkg, (lib : Odoc_unit.lib)) =
    if not (Hashtbl.mem compiled_libs lib.lib_name) then (
      Hashtbl.add compiled_libs lib.lib_name ();
      List.iter compile_lib (Plan.requires plan lib);
      Logs.debug (fun m -> m "Compiling library %s" lib.lib_name);
      Compile.compile_lib pkg lib)
  in
  fun (pkg : Odoc_unit.pkg) ->
    List.iter (fun lib -> compile_lib (pkg, lib)) pkg.libs

let compile_package pkg =
  compile_libs_of (Plan.of_packages [ pkg ]) pkg;
  Compile.compile_pages pkg

let all ~html_dir ~remaps ~generate_json ~warnings_tags
    (pkgs : Odoc_unit.pkg list) =
  let plan = Plan.of_packages pkgs in
  let compile_libs = compile_libs_of plan in
  let compiled_pkgs = Hashtbl.create 100 in
  let compile_pkg (pkg : Odoc_unit.pkg) =
    if not (Hashtbl.mem compiled_pkgs pkg.pkgname) then (
      Hashtbl.add compiled_pkgs pkg.pkgname ();
      compile_libs pkg;
      Compile.compile_pages pkg)
  in
  Compile.html_support html_dir;
  Compile.with_remaps remaps @@ fun remap_file ->
  List.iter
    (fun (pkg : Odoc_unit.pkg) ->
      compile_pkg pkg;
      List.iter compile_pkg (Plan.scope_packages plan pkg);
      Logs.debug (fun m -> m "Linking %a" (Fmt.option Fmt.string) pkg.pkgname);
      Compile.link ~warnings_tags pkg;
      Compile.generate ?remap_file ~generate_json html_dir pkg)
    (Plan.packages plan)
