(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

module Html = Tyxml.Html

let html_of_toc toc =
  let open Types in
  let rec section section =
    let link = Html.a ~a:[ Html.a_href section.href ] section.title in
    match section.children with [] -> [ link ] | cs -> [ link; sections cs ]
  and sections the_sections =
    the_sections
    |> List.map (fun the_section -> Html.li (section the_section))
    |> Html.ul
  in
  match toc with
  | [] -> []
  | _ -> [ Html.nav ~a:[ Html.a_class [ "odoc-toc" ] ] [ sections toc ] ]

let _json_of_toc toc =
  let open Types in
  let rec map { title_str; href; children; _ } =
    `Assoc
      [
        ("title", `String title_str);
        ("href", `String href);
        ("children", `List (List.map map children));
      ]
  in
  Yojson.Safe.to_string (`List (List.map map toc))

let page_creator ~config ~url name header toc content =
  let theme_uri = Config.theme_uri config in
  let support_uri = Config.support_uri config in
  let path = Link.Path.for_printing url in

  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let file_uri base file =
      match base with
      | Types.Absolute uri -> uri ^ "/" ^ file
      | Relative uri ->
          let page =
            Odoc_document.Url.Path.{ kind = `File; parent = uri; name = file }
          in
          Link.href ~config ~resolve:(Current url)
            (Odoc_document.Url.from_path page)
    in

    let odoc_css_uri = file_uri theme_uri "odoc.css" in
    let highlight_js_uri = file_uri support_uri "highlight.pack.js" in

    Html.head
      (Html.title (Html.txt title_string))
      [
        Html.link ~rel:[ `Stylesheet ] ~href:odoc_css_uri ();
        Html.meta ~a:[ Html.a_charset "utf-8" ] ();
        Html.meta
          ~a:[ Html.a_name "generator"; Html.a_content "odoc %%VERSION%%" ]
          ();
        Html.meta
          ~a:
            [
              Html.a_name "viewport";
              Html.a_content "width=device-width,initial-scale=1.0";
            ]
          ();
        Html.script ~a:[ Html.a_src highlight_js_uri ] (Html.txt "");
        Html.script (Html.txt "hljs.initHighlightingOnLoad();");
      ]
  in

  let gen_breadcrumbs () =
    let rec get_parents x =
      match x with
      | [] -> []
      | x :: xs -> (
          match Odoc_document.Url.Path.of_list (List.rev (x :: xs)) with
          | Some x -> x :: get_parents xs
          | None -> get_parents xs)
    in
    let parents =
      get_parents (List.rev (Odoc_document.Url.Path.to_list url)) |> List.rev
    in
    let href page =
      Link.href ~resolve:(Current url) (Odoc_document.Url.from_path page)
    in
    let make_navigation ~up_url breadcrumbs =
      [
        Html.nav
          ~a:[ Html.a_class [ "odoc-nav" ] ]
          ([
             Html.a ~a:[ Html.a_href up_url ] [ Html.txt "Up" ]; Html.txt " – ";
           ]
          @ breadcrumbs);
      ]
    in
    match parents with
    | [] -> [] (* Can't happen - Url.Path.to_list returns a non-empty list *)
    | [ _ ] -> [] (* No parents *)
    | [ x; { name = "index"; _ } ] ->
        (* Special case leaf pages called 'index' with one parent. This is for files called
           index.mld that would otherwise clash with their parent. In particular,
           dune and odig both cause this situation right now. *)
        let up_url = "../index.html" in
        let parent_name = x.name in
        make_navigation ~up_url [ Html.txt parent_name ]
    | _ ->
        let up_url = href ~config (List.hd (List.tl (List.rev parents))) in
        let l =
          (* Create breadcrumbs *)
          let space = Html.txt " " in
          parents
          |> Utils.list_concat_map
               ?sep:(Some [ space; Html.entity "#x00BB"; space ])
               ~f:(fun url' ->
                 [
                   [
                     (if url = url' then Html.txt url.name
                     else
                       Html.a
                         ~a:[ Html.a_href (href ~config url') ]
                         [ Html.txt url'.name ]);
                   ];
                 ])
          |> List.flatten
        in
        make_navigation ~up_url l
  in

  let breadcrumbs = if Config.omit_breadcrumbs config then [] else gen_breadcrumbs () in
  let body =
    breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ html_of_toc toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in
  Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body)

let make ~config ~url ~header ~toc title content children =
  let filename = Link.Path.as_filename ~is_flat:(Config.flat config) url in
  let html = page_creator ~config ~url title header toc content in
  let content ppf = (Html.pp ~indent:(Config.indent config) ()) ppf html in
  [ { Odoc_document.Renderer.filename; content; children } ]
