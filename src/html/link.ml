module Url = Odoc_document.Url

(* Translation from Url.Path *)
module Path = struct

  let to_list url =
    let rec loop acc {Url.Path. parent ; name ; kind } =
      match parent with
      | None -> (kind,name) :: acc
      | Some p -> loop ((kind, name) :: acc) p
    in
    loop [] url

  let for_printing url = List.map snd @@ to_list url

  let segment_to_string (kind, name) =
    if kind = "module" || kind = "page"
    then name
    else Printf.sprintf "%s-%s" kind name
  let for_linking url = List.map segment_to_string @@ to_list url

  let is_page url = (url.Url.Path.kind = "page")

  let rec get_dir {Url.Path. parent ; name ; kind} =
    let s = segment_to_string (kind, name) in
    match parent with
    | None -> Fpath.v s
    | Some p -> Fpath.(get_dir p / s)

  let as_filename (url : Url.Path.t) =
    (* if is_page url then
      Fpath.(get_dir url + ".html")
    else *)
      Fpath.(get_dir url / "index.html")
end

let semantic_uris = ref false

type resolve =
  | Current of Url.Path.t
  | Base of string

let rec drop_shared_prefix l1 l2 =
  match l1, l2 with
  | l1 :: l1s, l2 :: l2s when l1 = l2 ->
    drop_shared_prefix l1s l2s
  | _, _ -> l1, l2

(* let rec fmt_url_anchor fmt a =
  let open Url.Anchor in
  Format.fprintf fmt "{@[<v 2>@,page: %a@,anchor: %s@,kind: %s@]}" fmt_url_page a.page a.anchor a.kind
and fmt_url_page fmt p =
  let open Url.Path in
  Format.fprintf fmt "{@[<v 2>@,kind: %s@,parent: %a@,name: %s@]}" p.kind fmt_url_page_option p.parent p.name

and fmt_url_page_option fmt p =
  match p with
  | Some p -> fmt_url_page fmt p
  | None -> Format.fprintf fmt "None" *)
  
let href ~resolve t =
  let { Url.Anchor. page; anchor; _ } = t in
  let leaf list =
    let c l = String.concat "/" l in
    (* if kind = "page"
    then c list ^ ".html"
    else *)
      if !semantic_uris then c list else c (list @ ["index.html"])
  in
  let target = Path.for_linking page in

  let result = match resolve with
  (* If xref_base_uri is defined, do not perform relative URI resolution. *)
  | Base xref_base_uri ->
    (* Format.eprintf "base: \n%s\n%!" xref_base_uri; *)
    let page = xref_base_uri ^ leaf target in
    begin match anchor with
    | "" -> page
    | anchor -> page ^ "#" ^ anchor
    end
  | Current path ->
    (* Format.eprintf "current: \n%a\n%!" fmt_url_page path; *)
    let current_loc = Path.for_linking path in
    let current_from_common_ancestor, target_from_common_ancestor =
      drop_shared_prefix current_loc target
    in
    let relative_target =
      List.map (fun _ -> "..") current_from_common_ancestor
      @ target_from_common_ancestor
    in
    let page = leaf relative_target in
    begin match anchor with
    | "" -> page
    | anchor -> page ^ "#" ^ anchor
    end
    in
    (* Format.eprintf "url: %a\nresult: %s\n%!" fmt_url_anchor t result; *)

    result