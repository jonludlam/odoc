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

open StdLabels

let to_html_tree_page ?theme_uri ~syntax v =
  match syntax with
  | Html.Tree.Reason -> Html.Generator.Reason.page ?theme_uri v
  | Html.Tree.OCaml -> Html.Generator.ML.page ?theme_uri v

let to_html_tree_compilation_unit ?theme_uri ~syntax v =
  match syntax with
  | Html.Tree.Reason -> Html.Generator.Reason.compilation_unit ?theme_uri v
  | Html.Tree.OCaml -> Html.Generator.ML.compilation_unit ?theme_uri v

let from_odoc ~env ?(syntax=Html.Tree.OCaml) ?theme_uri ~output:root_dir input =
  let root = Root.read input in
  let is_linked = Fs.File.has_ext "odocl" input in
  match root.file with
  | Page page_name ->
    let odoctree = Page.load input in
    let odoctree =
      if is_linked
      then odoctree
      else Link.link_page ~env odoctree
    in
    let pkg_name = root.package in
    let pages = to_html_tree_page ?theme_uri ~syntax odoctree in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Html.Tree.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    )
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    let odoctree = Compilation_unit.load input in
    let odoctree =
      if is_linked
      then odoctree
      else Link.link_comp_unit ~env odoctree
    in 
    let pkg_dir =
      Fs.Directory.reach_from ~dir:root_dir root.package
    in
    let pages = to_html_tree_compilation_unit ?theme_uri ~syntax odoctree in
    Html.Tree.traverse pages ~f:(fun ~parents name content ->
      let directory =
        let dir =
          List.fold_right ~f:(fun name dir -> Fs.Directory.reach_from ~dir name)
            parents ~init:pkg_dir
        in
        Fs.Directory.reach_from ~dir name
      in
      let oc =
        Fs.Directory.mkdir_p directory;
        let file = Fs.File.create ~directory ~name:"index.html" in
        open_out (Fs.File.to_string file)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a@?" (Tyxml.Html.pp ()) content;
      close_out oc
    )

