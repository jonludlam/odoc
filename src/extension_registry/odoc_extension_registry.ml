(** Odoc Extension Registry

    This module provides a minimal registry for odoc tag extensions.
    It is kept separate to avoid circular dependencies between
    odoc_document and odoc_extension_api.
*)

module Comment = Odoc_model.Comment
module Location_ = Odoc_model.Location_

(** Resources that can be injected into the page (HTML only) *)
type resource =
  | Js_url of string
  | Css_url of string
  | Js_inline of string
  | Css_inline of string

(** Support files that extensions want to output *)
type support_file = {
  filename : string;  (** Relative path, e.g., "extensions/admonition.css" *)
  content : string;   (** File content *)
}

(** Result of processing a custom tag.
    We use a record with a polymorphic content type that gets
    instantiated with the actual Block.t by odoc_document. *)
type 'block extension_result = {
  content : 'block;
  overrides : (string * string) list;
  resources : resource list;
}

(** Type of handler functions stored in the registry.
    The handler takes a tag name and content, returns an optional result.
    If None, the tag is handled by the default mechanism. *)
type 'block handler =
  string ->  (* tag name *)
  Comment.nestable_block_element Location_.with_location list ->  (* content *)
  'block extension_result option

(** The registry stores handlers indexed by prefix *)
let handlers : (string, Obj.t) Hashtbl.t = Hashtbl.create 16

(** Registered prefixes for listing *)
let prefixes : (string, unit) Hashtbl.t = Hashtbl.create 16

(** Support files registered by extensions *)
let support_files : (string, support_file) Hashtbl.t = Hashtbl.create 16

let register_handler ~prefix (handler : 'block handler) =
  Hashtbl.replace handlers prefix (Obj.repr handler);
  Hashtbl.replace prefixes prefix ();
  Printf.printf "[odoc] Registered extension: @%s\n%!" prefix

let register_support_file ~prefix (file : support_file) =
  let key = prefix ^ ":" ^ file.filename in
  Hashtbl.replace support_files key file

let find_handler (type block) ~prefix : block handler option =
  match Hashtbl.find_opt handlers prefix with
  | None -> None
  | Some h -> Some (Obj.obj h)

let list_prefixes () =
  Hashtbl.fold (fun prefix () acc -> prefix :: acc) prefixes []
  |> List.sort String.compare

let list_support_files () =
  Hashtbl.fold (fun _ file acc -> file :: acc) support_files []

(** Extract the prefix from a tag name (part before the first dot) *)
let prefix_of_tag tag =
  match String.index_opt tag '.' with
  | None -> tag
  | Some i -> String.sub tag 0 i
