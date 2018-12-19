(*
 * Copyright (c) 2018 Jon Ludlam <jon@recoil.org>
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

let link_page ~env page =
  let resolve_env = Env.build env (`Page page) in
  Xref.resolve_page (Env.resolver resolve_env) page

let link_comp_unit ~env unit =
  let unit = Xref.Lookup.lookup unit in
  (* See comment in compile for explanation regarding the env duplication. *)
  let resolve_env = Env.build env (`Unit unit) in
  let resolved = Xref.resolve (Env.resolver resolve_env) unit in
  let expand_env = Env.build env (`Unit resolved) in
  Xref.expand (Env.expander expand_env) resolved
  |> Xref.Lookup.lookup
  |> Xref.resolve (Env.resolver expand_env) (* Yes, again. *)

let from_odoc ~env ~output ~input =
  let root = Root.read input in
  match root.file with
  | Page _page_name ->
    let page = Page.load input in
    let linked_page = link_page ~env page in
    Page.save output linked_page
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    let unit = Compilation_unit.load input in
    let linked_unit = link_comp_unit ~env unit in
    Compilation_unit.save output linked_unit
