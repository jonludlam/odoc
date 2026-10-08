type t = {
  name : string;
  code_blocks : (string * Odoc_document.Extension.code_block_handler) list;
  raw_markup : (string * Odoc_document.Extension.raw_markup_handler) list;
  tags : (string * Odoc_document.Extension.tag_handler) list;
  head : Odoc_html.Extension.head option;
  support_files : (string * string) list;
}

let v ~name =
  {
    name;
    code_blocks = [];
    raw_markup = [];
    tags = [];
    head = None;
    support_files = [];
  }

(* The first two extensions that claim the same key, if any. *)
let clash ~what ~key extensions keys =
  let claims =
    List.concat_map
      (fun e -> List.map (fun k -> (key k, e.name)) (keys e))
      extensions
  in
  let rec find = function
    | [] -> None
    | (k, a) :: rest -> (
        match List.assoc_opt k rest with
        | Some b ->
            Some
              (Printf.sprintf "Extensions %s and %s both handle %s %s." a b what
                 k)
        | None -> find rest)
  in
  find claims

(* The first key of an extension that odoc keeps for itself, if any. *)
let reserved ~what ~key ~is_reserved extensions keys =
  List.find_map
    (fun e ->
      List.find_map
        (fun k ->
          if is_reserved (key k) then
            Some
              (Printf.sprintf "Extension %s cannot %s %s." e.name what (key k))
          else None)
        (keys e))
    extensions

let register extensions =
  let clashes =
    [
      reserved ~what:"handle the raw markup for"
        ~key:(fun (t, _) -> String.lowercase_ascii t)
        ~is_reserved:(fun t -> List.mem t [ "html"; "latex"; "man" ])
        extensions
        (fun e -> e.raw_markup);
      reserved ~what:"handle the tags" ~key:fst
        ~is_reserved:(fun p -> not (Odoc_model.Custom_tags.is_valid_prefix p))
        extensions
        (fun e -> e.tags);
      reserved ~what:"replace odoc's support file" ~key:fst
        ~is_reserved:(fun p -> List.mem p Odoc_html_support_files.file_list)
        extensions
        (fun e -> e.support_files);
      clash ~what:"code blocks in" ~key:fst extensions (fun e -> e.code_blocks);
      clash ~what:"the raw markup for"
        ~key:(fun (t, _) -> String.lowercase_ascii t)
        extensions
        (fun e -> e.raw_markup);
      clash ~what:"the tags" ~key:fst extensions (fun e -> e.tags);
      clash ~what:"the support file" ~key:fst extensions (fun e ->
          e.support_files);
    ]
  in
  match List.find_map Fun.id clashes with
  | Some msg -> Error msg
  | None ->
      List.iter
        (fun e ->
          List.iter
            (fun (language, h) ->
              Odoc_document.Extension.register_code_block ~language h)
            e.code_blocks;
          List.iter
            (fun (target, h) ->
              Odoc_document.Extension.register_raw_markup ~target h)
            e.raw_markup;
          List.iter
            (fun (prefix, h) -> Odoc_document.Extension.register_tag ~prefix h)
            e.tags;
          Option.iter Odoc_html.Extension.register_head e.head;
          List.iter
            (fun (path, content) ->
              Odoc_html.Extension.register_support_file ~path content)
            e.support_files)
        extensions;
      Ok ()
