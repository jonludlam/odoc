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

  (** Split on any of [\n] or [\r], dropping empty fields. Useful for parsing
      line-separated text files where line endings are either flavour. *)
  let split_lines s =
    let is_sep = function '\n' | '\r' -> true | _ -> false in
    let n = String.length s in
    let rec loop acc i =
      if i >= n then List.rev acc
      else if is_sep s.[i] then loop acc (i + 1)
      else
        let rec find_end j =
          if j >= n || is_sep s.[j] then j else find_end (j + 1)
        in
        let j = find_end (i + 1) in
        loop (String.sub s i (j - i) :: acc) j
    in
    loop [] 0
end

module Path = struct
  let dir_sep = Filename.dir_sep

  let dir_sep_char = Filename.dir_sep.[0]

  let of_string s =
    if s = "" then Error (`Msg "empty path")
    else Ok s

  let v s =
    match of_string s with
    | Ok p -> p
    | Error (`Msg e) -> invalid_arg ("Odoc_utils.Path.v: " ^ e)

  let to_string p = p

  let pp = Format.pp_print_string

  let basename = Filename.basename
  let dirname = Filename.dirname

  (** Like Fpath.parent: returns the directory portion with a trailing
      separator. Equivalent to [Filename.dirname p ^ "/"]. *)
  let parent p =
    let d = Filename.dirname p in
    if d = "" || d.[String.length d - 1] = dir_sep_char then d
    else d ^ dir_sep

  let append = Filename.concat
  let concat = Filename.concat

  let is_relative = Filename.is_relative

  let extension p = Filename.extension p

  let has_ext ~ext p =
    let want =
      if String.length ext > 0 && ext.[0] = '.' then ext else "." ^ ext
    in
    Filename.extension p = want

  let get_ext = Filename.extension

  let rem_ext = Filename.remove_extension

  let add_ext ext p =
    let dot_ext =
      if String.length ext > 0 && ext.[0] = '.' then ext else "." ^ ext
    in
    p ^ dot_ext

  let set_ext ext p = add_ext ext (Filename.remove_extension p)

  (** Split path into segments, dropping empty segments from consecutive
      separators and trailing separators. *)
  let segs p =
    String.split_on_char dir_sep_char p
    |> List.filter (fun s -> s <> "")

  (** Normalize: collapse "." and ".." segments, dedupe slashes. *)
  let normalize p =
    if p = "" then "."
    else
      let absolute = p.[0] = dir_sep_char in
      let parts = segs p in
      let rec go acc = function
        | [] -> List.rev acc
        | "." :: rest -> go acc rest
        | ".." :: rest ->
            (match acc with
             | top :: tl when top <> ".." -> go tl rest
             | [] when absolute -> go [] rest
             | _ -> go (".." :: acc) rest)
        | s :: rest -> go (s :: acc) rest
      in
      let normalized = go [] parts in
      let body = String.concat dir_sep normalized in
      if absolute then dir_sep ^ body
      else if body = "" then "."
      else body

  let is_prefix ~root p =
    let r = normalize root in
    let p = normalize p in
    let rlen = String.length r in
    String.length p >= rlen
    && String.starts_with ~prefix:r p
    && (String.length p = rlen || p.[rlen] = dir_sep_char
        || (rlen > 0 && r.[rlen - 1] = dir_sep_char))

  let rem_prefix root p =
    if is_prefix ~root p then
      let r = normalize root in
      let p = normalize p in
      let rlen = String.length r in
      if String.length p = rlen then Some "."
      else
        let start =
          if rlen > 0 && r.[rlen - 1] = dir_sep_char then rlen else rlen + 1
        in
        if start >= String.length p then Some "."
        else Some (String.sub p start (String.length p - start))
    else None

  let relativize ~root p = rem_prefix root p

  let split_base p = (Filename.dirname p, Filename.basename p)

  let to_dir_path p =
    if p = "" || p.[String.length p - 1] = dir_sep_char then p
    else p ^ dir_sep

  let rem_empty_seg p =
    let n = String.length p in
    if n > 1 && p.[n - 1] = dir_sep_char then String.sub p 0 (n - 1)
    else p

  let filename = Filename.basename

  let compare = String.compare
  let equal = String.equal

  module Set = Set.Make (String)
  module Map = Map.Make (String)
end
