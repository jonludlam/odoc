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
   mode exposes as [--actions compile-only] and [--actions link-and-gen]. A
   scope holds names, since a name is what goes on the command line; one no
   package of this run answers belongs to a package an earlier run compiled,
   or to no package at all.

   The top-level index, whose scope is every package, is expected last. *)

(* Compiling a package's libraries, each after the libraries it requires and
   each at most once. A library pulled in by another package's scope is
   compiled once, since the memo is shared by the whole run. *)
let compile_libs () =
  let compile_lib =
    Util.memo ~key:(fun (lib : Odoc_unit.lib) -> lib.lib_name)
    @@ fun compile_lib lib ->
    List.iter compile_lib lib.requires;
    Logs.debug (fun m -> m "Compiling library %s" lib.lib_name);
    Compile.compile_lib lib
  in
  fun (pkg : Odoc_unit.pkg) -> List.iter compile_lib pkg.libs

let compile_package pkg =
  compile_libs () pkg;
  Compile.compile_pages pkg

let all ~html_dir ~remaps ~generate_json ~warnings_tags
    (pkgs : Odoc_unit.pkg list) =
  let scope_pkgs = Odoc_unit.scope_pkgs pkgs in
  let compile_libs = compile_libs () in
  let compile_pkg =
    Util.memo ~key:(fun (pkg : Odoc_unit.pkg) -> pkg.pkgname) @@ fun _ pkg ->
    compile_libs pkg;
    Compile.compile_pages pkg
  in
  Compile.html_support html_dir;
  Compile.with_remaps remaps @@ fun remap_file ->
  List.iter
    (fun (pkg : Odoc_unit.pkg) ->
      compile_pkg pkg;
      List.iter compile_pkg (scope_pkgs pkg);
      Logs.debug (fun m -> m "Linking %a" (Fmt.option Fmt.string) pkg.pkgname);
      Compile.link ~warnings_tags pkg;
      Compile.generate ?remap_file ~generate_json html_dir pkg)
    pkgs
