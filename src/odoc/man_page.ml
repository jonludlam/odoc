
open Or_error
open Odoc_document

let document_of_page ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.page v
  | Renderer.OCaml -> Odoc_document.ML.page v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.compilation_unit v
  | Renderer.OCaml -> Odoc_document.ML.compilation_unit v

let mk_page ~syntax v =
  Odoc_manpage.Generator.render @@
  document_of_page ~syntax v

let mk_compilation_unit ~syntax v =
  Odoc_manpage.Generator.render @@
  document_of_compilation_unit ~syntax v

let from_odoc ~env ?(syntax=Renderer.OCaml) ~output:root_dir input =
  Root.read input >>= fun root ->
  let input_s = Fs.File.to_string input in
  match root.file with
  | Page _ ->
    Page.load input >>= fun page ->
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Odoc_xref2.Link.resolve_page resolve_env page
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    let pages = mk_page ~syntax odoctree in
    Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      Format.eprintf "%s@." (Fpath.to_string filename);
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    Compilation_unit.load input >>= fun unit ->
    let odoctree =
      let env = Env.build env (`Unit unit) in
      Odoc_xref2.Link.link env unit
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir root.package in
    Fs.Directory.mkdir_p pkg_dir;
    let pages = mk_compilation_unit ~syntax odoctree in
    Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      Format.eprintf "%s@." (Fpath.to_string filename);
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
