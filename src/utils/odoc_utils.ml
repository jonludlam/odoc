type msg = [ `Msg of string ]

(** The [result] type and a bind operator. This module is meant to be opened. *)
module ResultMonad = struct
  let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

  let of_option ~error = function Some x -> Ok x | None -> Error error

  let ( >>= ) = Result.bind
end

(** A bind operator for the [option] type. This module is meant to be opened. *)
module OptionMonad = struct
  (* The error case become [None], the error value is ignored. *)
  let of_result = function Ok x -> Some x | Error _ -> None

  let ( >>= ) = Option.bind
end

module List = Odoc_list

module Tree = Tree
module Forest = Tree.Forest
module Json = Json

module Io_utils = struct
  (** [with_open_*] are resource safe wrappers around opening and closing
      channels. They are equivalent to the same functions in OCaml 4.14's
      [In_channel] and [Out_channel]. *)

  let _with_resource res ~close f =
    Fun.protect ~finally:(fun () -> close res) (fun () -> f res)

  let with_open_in fname f =
    _with_resource (open_in fname) ~close:close_in_noerr f

  let with_open_in_bin fname f =
    _with_resource (open_in_bin fname) ~close:close_in_noerr f

  (** Read a file line-by-line by folding [f]. *)
  let fold_lines fname f acc =
    _with_resource (open_in fname) ~close:close_in_noerr (fun ic ->
        let rec loop acc =
          match input_line ic with
          | exception End_of_file -> acc
          | line -> loop (f line acc)
        in
        loop acc)

  (** Read a file as a list of lines. *)
  let read_lines fname =
    List.rev (fold_lines fname (fun line acc -> line :: acc) [])

  let with_open_out fname f =
    _with_resource (open_out fname) ~close:close_out_noerr f

  let with_open_out_bin fname f =
    _with_resource (open_out_bin fname) ~close:close_out_noerr f

  (** Like [with_open_out] but operate on a [Format] buffer. *)
  let with_formatter_out fname f =
    with_open_out fname (fun oc -> f (Format.formatter_of_out_channel oc))

  (** Shortcuts for composing [with_open_*] functions and [Marshal]. *)
  let marshal fname v =
    with_open_out_bin fname (fun oc -> Marshal.to_channel oc v [])

  let unmarshal fname = with_open_in_bin fname Marshal.from_channel
end

module Int = struct
  include Int
  let max x y : t = if x >= y then x else y
end

module String = struct
  include String

  let cut ~sep s =
    let slen = String.length sep in
    if slen = 0 then invalid_arg "String.cut: empty separator"
    else
      let n = String.length s in
      let first = String.unsafe_get sep 0 in
      let rec find i =
        if i + slen > n then None
        else if String.unsafe_get s i = first && String.sub s i slen = sep then
          Some i
        else find (i + 1)
      in
      match find 0 with
      | None -> None
      | Some j ->
          Some (String.sub s 0 j, String.sub s (j + slen) (n - j - slen))

  let cut_right ~sep s =
    let slen = String.length sep in
    if slen = 0 then invalid_arg "String.cut_right: empty separator"
    else
      let n = String.length s in
      let first = String.unsafe_get sep 0 in
      let rec find i =
        if i < 0 then None
        else if String.unsafe_get s i = first && String.sub s i slen = sep then
          Some i
        else find (i - 1)
      in
      match find (n - slen) with
      | None -> None
      | Some j ->
          Some (String.sub s 0 j, String.sub s (j + slen) (n - j - slen))

  let cuts ~sep s =
    let slen = String.length sep in
    if slen = 0 then invalid_arg "String.cuts: empty separator"
    else
      let rec loop acc s =
        match cut ~sep s with
        | None -> List.rev (s :: acc)
        | Some (before, rest) -> loop (before :: acc) rest
      in
      loop [] s

  let is_infix ~affix s =
    let alen = String.length affix in
    let slen = String.length s in
    if alen = 0 then true
    else if alen > slen then false
    else
      let first = String.unsafe_get affix 0 in
      let rec scan i =
        if i + alen > slen then false
        else if String.unsafe_get s i = first && String.sub s i alen = affix
        then true
        else scan (i + 1)
      in
      scan 0
end
