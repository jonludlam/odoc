(* Build packages one at a time.

   {!Plan} works out the order: a library after the libraries it requires,
   and a package linked after every package its reference scope names is
   compiled. That second rule is what odoc-config.sexp needs, since it can
   put a package built later in the scope: eio's documentation points at
   eio_main, odoc's at odoc-driver. It is also the split that voodoo mode
   exposes as [--actions compile-only] and [--actions link-and-gen].

   Running a step is all that is left here. *)

let compile_step = function
  | Plan.Compile_lib (pkg, (lib : Odoc_unit.lib)) ->
      Logs.debug (fun m -> m "Compiling library %s" lib.lib_name);
      Compile.compile_lib pkg lib
  | Plan.Compile_pages pkg -> Compile.compile_pages pkg
  | Plan.Link _ -> ()

let compile_package pkg = List.iter compile_step (Plan.compile_only pkg)

let all ~html_dir ~remaps ~generate_json ~warnings_tags
    (pkgs : Odoc_unit.pkg list) =
  let plan = Plan.of_packages pkgs in
  Compile.html_support html_dir;
  Compile.with_remaps remaps @@ fun remap_file ->
  List.iter
    (function
      | Plan.Link (pkg : Odoc_unit.pkg) ->
          Logs.debug (fun m ->
              m "Linking %a" (Fmt.option Fmt.string) pkg.pkgname);
          Compile.link ~warnings_tags pkg;
          Compile.generate ?remap_file ~generate_json html_dir pkg
      | step -> compile_step step)
    plan
