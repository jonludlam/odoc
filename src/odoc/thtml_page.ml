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

open Odoc_document

type args = {
  semantic_uris : bool;
  closed_details : bool;
  indent : bool;
  header : Fpath.t option;
  theme_uri : Odoc_thtml.Tree.uri;
}

let render args page =
  (match args.header with
  | Some f ->
      let ic = open_in (Fpath.to_string f) in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Odoc_thtml.Tree.hdr := Some s
  | None -> ());
  Odoc_thtml.Link.semantic_uris := args.semantic_uris;
  Odoc_thtml.Tree.open_details := not args.closed_details;
  Odoc_thtml.Generator.render ~theme_uri:args.theme_uri ~indent:args.indent page

let files_of_url url = [ Odoc_thtml.Link.Path.as_filename url ]

let renderer = { Renderer.name = "thtml"; render; files_of_url }
