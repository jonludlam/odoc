(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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

let to_html_tree_page ?theme_uri ~syntax v =
  Odoc_html.Generator.render ?theme_uri @@
  document_of_page ~syntax v

let to_html_tree_compilation_unit ?theme_uri ~syntax v =
  Odoc_html.Generator.render ?theme_uri @@
  document_of_compilation_unit ~syntax v

let from_odoc ~env ?(syntax=Renderer.OCaml) ?theme_uri ~output:root_dir input =
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
    Page.save Fs.File.(set_ext ".odocl" input) odoctree;
    let pages = to_html_tree_page ?theme_uri ~syntax odoctree in
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
  | Compilation_unit {hidden; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    Compilation_unit.load input >>= fun unit ->
    let unit =
      if hidden
      then {unit with content = Odoc_model.Lang.Compilation_unit.Module []; expansion=None }
      else unit
    in
    let odoctree =
      let env = Env.build env (`Unit unit) in
      (* let startlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Link...\n%!"; *)
      let linked = Odoc_xref2.Link.link env unit in
      (* let finishlink = Unix.gettimeofday () in *)
      (* Format.fprintf Format.err_formatter "**** Finished: Link=%f\n%!" (finishlink -. startlink); *)
      (* Printf.fprintf stderr "num_times: %d\n%!" !Odoc_xref2.Tools.num_times; *)
      linked
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    
    Odoc_xref2.Tools.reset_caches ();
    Hashtbl.clear Compilation_unit.units_cache;

    Compilation_unit.save Fs.File.(set_ext ".odocl" input) odoctree;

    let pages = to_html_tree_compilation_unit ?theme_uri ~syntax odoctree in
    Renderer.traverse pages ~f:(fun filename content ->
      let filename = Fpath.normalize @@ Fs.File.append root_dir filename in
      let directory = Fs.File.dirname filename in
      Fs.Directory.mkdir_p directory;
      let oc = open_out (Fs.File.to_string filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );

    Ok ()

(* Used only for [--index-for] which is deprecated and available only for
   backward compatibility. It should be removed whenever. *)
let from_mld ~env ?(syntax=Renderer.OCaml) ~package ~output:root_dir ~warn_error input =
  Odoc_model.Error.set_warn_error warn_error;
  let root_name = "index" in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page root_name in
    {Odoc_model.Root.package; file; digest}
  in
  let name = `Page (root, Odoc_model.Names.PageName.of_string root_name) in
  let location =
    let pos =
      Lexing.{
        pos_fname = input_s;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  let to_html content =
    (* This is a mess. *)
    let page = Odoc_model.Lang.Page.{ name; content; digest } in
(*    let page = Odoc_xref.Lookup.lookup_page page in*)
    let env = Env.build env (`Page page) in
    let resolved =
      Odoc_xref2.Link.resolve_page env page
      |> Odoc_xref2.Lookup_failures.to_warning ~filename:input_s
      |> Odoc_model.Error.shed_warnings
    in
    let pages = to_html_tree_page ~syntax resolved in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir root.package in
    Fs.Directory.mkdir_p pkg_dir;
    Renderer.traverse pages ~f:(fun _pkg_name content ->
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:"index.html" in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
  in
  match Fs.File.read input with
  | Error _ as e -> e
  | Ok str ->
    match Odoc_loader.read_string name location str with
    | Error e -> Error (`Msg (Odoc_model.Error.to_string e))
    | Ok (`Docs content) -> to_html content
    | Ok `Stop -> to_html [] (* TODO: Error? *)
