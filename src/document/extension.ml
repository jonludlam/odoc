module Location_ = Odoc_model.Location_

(* The page being generated, if any. *)
let current_page : Url.Path.t option ref = ref None

let in_page url f =
  let outer = !current_page in
  current_page := Some url;
  Fun.protect ~finally:(fun () -> current_page := outer) f

let current_backend = ref "html"

let for_backend backend f =
  let outer = !current_backend in
  current_backend := backend;
  Fun.protect ~finally:(fun () -> current_backend := outer) f

(* Kept by page, rather than in the context, so that a page whose generation
   is interrupted by another one's carries on where it was. *)
let ids : (Url.Path.t option * string, int) Hashtbl.t = Hashtbl.create 8

let requirements : (Url.Path.t, string list) Hashtbl.t = Hashtbl.create 8

let required url = Option.value ~default:[] (Hashtbl.find_opt requirements url)

module Context = struct
  type t = {
    url : Url.Path.t option;
    location : Location_.span;
    backend : string;
  }

  let url t = t.url

  let backend t = t.backend

  let location t = t.location

  (* Outside a page, for example in the search index, the element is rendered
     again, and its warnings were already reported when its page was. *)
  let warn t msg =
    if Option.is_some t.url then
      Odoc_model.Error.raise_warning (Odoc_model.Error.make "%s" msg t.location)

  let fresh_id t prefix =
    let key = (t.url, prefix) in
    let n = 1 + Option.value ~default:0 (Hashtbl.find_opt ids key) in
    Hashtbl.replace ids key n;
    Printf.sprintf "%s-%d" prefix n

  let require t feature =
    match t.url with
    | None -> ()
    | Some url ->
        let features = required url in
        if not (List.mem feature features) then
          Hashtbl.replace requirements url (features @ [ feature ])
end

let context location =
  { Context.url = !current_page; location; backend = !current_backend }

type code_block_handler =
  Context.t -> Odoc_model.Comment.code_block -> Types.Block.t option

let code_block_handlers : (string, code_block_handler) Hashtbl.t =
  Hashtbl.create 8

let register_code_block ~language handler =
  Hashtbl.replace code_block_handlers language handler

let render_code_block ~language ~location code_block =
  match Hashtbl.find_opt code_block_handlers language with
  | Some handler -> handler (context location) code_block
  | None -> None
