open Result

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

let dump_sexp unit filename =
  let sexp = Odoc__print.Print.Lang.sexp_of_compilation_unit_t unit in
  let oc = open_out (Fs.File.to_string filename) in
  output_string oc (Sexplib.Sexp.to_string_hum sexp);
  close_out oc

let dump_sexp_page page filename =
  let sexp = Odoc__print.Print.Lang.sexp_of_page_t page in
  let oc = open_out (Fs.File.to_string filename) in
  output_string oc (Sexplib.Sexp.to_string_hum sexp);
  close_out oc

let resolve_and_substitute ~env ~output input_file read_file =
  let filename = Fs.File.to_string input_file in
  match read_file ~filename:filename with
  | Error e -> failwith (Model.Error.to_string e)
  | Ok unit ->
    dump_sexp unit (Fs.File.set_ext "0.before.sexp" input_file);
    let unit = Xref.Lookup.lookup unit in
    let f2 = Fs.File.set_ext "1.post_lookup.sexp" input_file in
    dump_sexp unit f2;
    if not unit.Model.Lang.Compilation_unit.interface then (
      Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
        (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
         else
          Printf.sprintf
            " Using %S while you should use the .cmti file" filename)
    );
    let resolve_env = Env.build env (`Unit unit) in
    let resolved = Xref.resolve (Env.resolver resolve_env) unit in
    let f2 = Fs.File.set_ext "2.post_resolve.sexp" input_file in
    dump_sexp resolved f2;

    (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
       defined, elements. We'd rather it got back the resolved bit so we rebuild an
       environment with the resolved unit.
       Note that this is bad and once rewritten expand should not fetch the unit it is
       working on. *)
    let expand_env = Env.build env (`Unit resolved) in
    let expanded = Xref.expand (Env.expander expand_env) resolved in
    let f2 = Fs.File.set_ext "3.post_expand.sexp" input_file in
    dump_sexp expanded f2;
    Compilation_unit.save output expanded

let root_of_compilation_unit ~package ~hidden ~module_name ~digest =
  let file_representation : Model.Root.Odoc_file.t =
    Model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Model.Root.package; file = file_representation; digest}

let cmti ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmti ~make_root in
  resolve_and_substitute ~env ~output input read_file

let cmt ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmt ~make_root in
  resolve_and_substitute ~env ~output input read_file

let cmi ~env ~package ~hidden ~output input =
  let make_root = root_of_compilation_unit ~package ~hidden in
  let read_file = Loader.read_cmi ~make_root in
  resolve_and_substitute ~env ~output input read_file

(* TODO: move most of this to doc-ock. *)
let mld ~env ~package ~output input =
  let root_name =
    let page_dash_root =
      Filename.chop_extension (Fs.File.(to_string @@ basename output))
    in
    String.sub page_dash_root (String.length "page-")
      (String.length page_dash_root - String.length "page-")
  in
  let digest = Digest.file (Fs.File.to_string input) in
  let root =
    let file = Model.Root.Odoc_file.create_page root_name in
    {Model.Root.package; file; digest}
  in
  let name = `Page (root, Model.Names.PageName.of_string root_name) in
  let location =
    let pos =
      Lexing.{
        pos_fname = Fs.File.to_string input;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  match Fs.File.read input with
  | Error (`Msg s) ->
    Printf.eprintf "ERROR: %s\n%!" s;
    exit 1
  | Ok str ->
    let content =
      match Loader.read_string name location str with
      | Error e -> failwith (Model.Error.to_string e)
      | Ok (`Docs content) -> content
      | Ok `Stop -> [] (* TODO: Error? *)
    in
    (* This is a mess. *)

    let page = Model.Lang.Page.{ name; content; digest } in
    dump_sexp_page page (Fs.File.set_ext "0.initial.sexp" input);

    let page = Xref.Lookup.lookup_page page in
    dump_sexp_page page (Fs.File.set_ext "1.post_lookup.sexp" input);

    let env = Env.build env (`Page page) in
    let resolved = Xref.resolve_page (Env.resolver env) page in
    dump_sexp_page page (Fs.File.set_ext "2.post_resolve.sexp" input);

    Page.save output resolved
