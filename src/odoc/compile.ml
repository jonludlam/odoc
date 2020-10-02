open Or_error

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


let parent parent_file_opt package_opt =
  match parent_file_opt, package_opt with
  | Some f, _ -> begin
    let r = Root.read f in
    match r with
    | Ok r -> begin
      match r.file with
        | Odoc_model.Root.Odoc_file.Page s -> begin
          let pagename = Odoc_model.Names.PageName.of_string s in
          match r.parent with
          | None -> Some (`Page pagename)
          | Some p -> Some (`SubPage (p, pagename))
        end
        | _ -> failwith "Expecting page"
     end
      | Error _e ->
        failwith "Failed to read parent page"
    end
  | None, Some package -> Some (`Page (Odoc_model.Names.PageName.of_string package))
  | None, None -> None

let resolve_and_substitute ~env ~output ~warn_error parent input_file read_file =
  let filename = Fs.File.to_string input_file in

  read_file ~parent ~filename |> Odoc_model.Error.handle_errors_and_warnings ~warn_error >>= fun unit ->

  if not unit.Odoc_model.Lang.Compilation_unit.interface then (
    Printf.eprintf "WARNING: not processing the \"interface\" file.%s\n%!"
      (if not (Filename.check_suffix filename "cmt") then "" (* ? *)
       else
        Printf.sprintf
          " Using %S while you should use the .cmti file" filename)
  );
  let env = Env.build env (`Unit unit) in

  Odoc_xref2.Compile.compile env unit
  |> Odoc_xref2.Lookup_failures.handle_failures ~warn_error ~filename
  >>= fun compiled ->

  (* [expand unit] fetches [unit] from [env] to get the expansion of local, previously
     defined, elements. We'd rather it got back the resolved bit so we rebuild an
     environment with the resolved unit.
     Note that this is bad and once rewritten expand should not fetch the unit it is
     working on. *)
(*    let expand_env = Env.build env (`Unit resolved) in*)
(*    let expanded = Odoc_xref2.Expand.expand (Env.expander expand_env) resolved in *)
  Compilation_unit.save output compiled;
  Ok ()

let root_of_compilation_unit ~parent ~hidden ~module_name ~digest =
  let file_representation : Odoc_model.Root.Odoc_file.t =
    Odoc_model.Root.Odoc_file.create_unit ~force_hidden:hidden module_name in
  {Odoc_model.Root.parent = Some parent; file = file_representation; digest}

let mld ~parent_opt ~output ~warn_error input =
  let root_name =
    let page_dash_root =
      Filename.chop_extension (Fs.File.(to_string @@ basename output))
    in
    String.sub page_dash_root (String.length "page-")
      (String.length page_dash_root - String.length "page-")
  in
  let input_s = Fs.File.to_string input in
  let digest = Digest.file input_s in
  let root =
    let file = Odoc_model.Root.Odoc_file.create_page root_name in
    {Odoc_model.Root.parent = parent_opt; file; digest}
  in
  let page_name = Odoc_model.Names.PageName.of_string root_name in
  let name =
    match parent_opt with
    | Some p -> `SubPage (p, page_name )
    | None -> `Page (page_name) in
  let resolve content =
    let page = Odoc_model.Lang.Page.{ name; root; content; digest } in
    Page.save output page;
    Ok ()
  in
  Fs.File.read input >>= fun str ->
  Odoc_loader.read_string name input_s str |> Odoc_model.Error.handle_errors_and_warnings ~warn_error
  >>= function
  | `Stop -> resolve [] (* TODO: Error? *)
  | `Docs content -> resolve content

let compile ~env ~parent_file_opt ~package_opt ~hidden ~output ~warn_error input =
  let parent_opt = parent parent_file_opt package_opt in
  let ext = Fs.File.get_ext input in
  if ext = ".mld"
  then mld ~parent_opt ~output ~warn_error input
  else
    (match ext with
      | ".cmti" -> Ok Odoc_loader.read_cmti
      | ".cmt" -> Ok Odoc_loader.read_cmt
      | ".cmi" -> Ok Odoc_loader.read_cmi
      | _ -> Error (`Msg "Unknown extension, expected one of: cmti, cmt, cmi or mld.")) >>= fun loader ->
    let parent = match parent_opt with | Some p -> p | None -> failwith "Compilation_units require a parent" in
    let make_root = root_of_compilation_unit ~parent ~hidden in
    resolve_and_substitute ~env ~output ~warn_error parent input (loader ~make_root)
