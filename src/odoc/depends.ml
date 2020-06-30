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
open Or_error

module Compile = struct
  type t = {
    unit_name : string;
    digest : Digest.t;
  }

  let name t = t.unit_name
  let digest t = t.digest
end

let add_dep acc = function
| _, None -> acc (* drop module aliases *)
| unit_name, Some digest ->  { Compile.unit_name; digest } :: acc

let is_hidden s =
  let len = String.length s in
  let rec aux i =
      if i > len - 2 then false else
      if s.[i] = '_' && s.[i + 1] = '_' then true
      else aux (i + 1)
  in aux 0 

let rec read_depends package cur =
  let deps =
    try
      let ic = open_in ("deps/"^package) in
      let rec inner () =
        try
          let l = input_line ic in
          l :: inner ()
        with _ -> []
      in
      let deps = inner () in
      close_in ic;
      deps
    with _ -> []
  in
  let new_deps = List.filter (fun l -> not @@ List.mem l cur) deps in
  let trans_deps = package :: (List.flatten @@ deps :: List.map (fun dep -> read_depends dep (new_deps @ cur)) new_deps) in
  let rec uniq x =
    match x with
    | l :: ls -> if List.mem l ls then uniq ls else l::uniq ls
    | [] -> []
  in uniq trans_deps
    

let makefile_fragment file name digest deps package =
  let outputfilestr = Fs.File.(set_ext ".odoc" file |> basename |> to_string) in
  let deps = List.filter (fun dep -> dep.Compile.unit_name <> name) deps in
  let deps = List.map (fun dep -> Printf.sprintf "%s.odoc" (Digest.to_hex dep.Compile.digest)) deps in
  let pkg_deps_str = String.concat " " (List.map (fun dep -> "-I " ^ dep) (read_depends package [])) in
  Format.printf "%s.odoc : %s/%s\n" (Digest.to_hex digest) package outputfilestr;
  Format.printf "\tln -sf $< %s\n" "$@";
  Format.printf "%s/%s : %s %s\n%!" package outputfilestr (Fs.File.to_string file)
    (String.concat " " deps);
  Format.printf "\todoc compile --package %s $< %s -o %s\n" package pkg_deps_str "$@";
  Format.printf "allcompile : %s.odoc\n" (Digest.to_hex digest);
  if not (is_hidden name)
  then begin
    Format.printf "%s.odocl : %s.odoc\n\todoc link $< %s\n" (Digest.to_hex digest) (Digest.to_hex digest) pkg_deps_str;
    Format.printf "%s.odocl.generated: %s.odocl\n\todoc generate $< --output-dir output\n\ttouch %s\n%!" (Digest.to_hex digest) (Digest.to_hex digest) "$@";
    Format.printf "alllink : %s.odocl.generated\n" (Digest.to_hex digest);
  end


let for_compile_step_cmt file package =
  let cmt_infos = Cmt_format.read_cmt (Fs.File.to_string file) in
  let deps = List.fold_left ~f:add_dep ~init:[] cmt_infos.Cmt_format.cmt_imports in
  let name = cmt_infos.cmt_modname in
  let digest =
    match cmt_infos.cmt_interface_digest with
    | Some d -> d
    | None ->
      match List.assoc name cmt_infos.cmt_imports with
      | Some digest -> digest 
      | _ -> failwith "Invalid file"
  in
  makefile_fragment file name digest deps package;
  []

let for_compile_step_cmi_or_cmti file package =
  let cmi_infos = Cmi_format.read_cmi (Fs.File.to_string file) in
  match cmi_infos.cmi_crcs with
  | (name, Some digest) :: imports when name = cmi_infos.cmi_name ->
    let deps = List.fold_left ~f:add_dep ~init:[] imports in
    makefile_fragment file name digest deps package;
    []
  | _ -> failwith "Error"

let for_compile_step file package = match Fs.File.has_ext "cmt" file with
| true -> for_compile_step_cmt file package
| false -> for_compile_step_cmi_or_cmti file package

module Hash_set : sig
  type t

  val create : unit -> t

  val add : t -> Odoc_model.Root.t -> unit

  val elements : t -> Odoc_model.Root.t list
end = struct
  type t = unit Odoc_model.Root.Hash_table.t

  let add t elt =
    if Odoc_model.Root.Hash_table.mem t elt then
      ()
    else
      Odoc_model.Root.Hash_table.add t elt ()

  let create () = Odoc_model.Root.Hash_table.create 42

  let elements t = Odoc_model.Root.Hash_table.fold (fun s () acc -> s :: acc) t []
end

let deps_of_odoc_file ~deps input =
  Root.read input >>= function
  | { file = Page _; _ } -> Ok () (* XXX something should certainly be done here *)
  | { file = Compilation_unit _; _ } ->
    Compilation_unit.load input >>= fun odoctree ->
    List.iter odoctree.Odoc_model.Lang.Compilation_unit.imports ~f:(fun import ->
        match import with
        | Odoc_model.Lang.Compilation_unit.Import.Unresolved _  -> ()
        | Odoc_model.Lang.Compilation_unit.Import.Resolved root ->
            Hash_set.add deps root
      );
    Ok ()

let for_html_step pkg_dir =
  let deps = Hash_set.create () in
  let add_deps () file = deps_of_odoc_file ~deps file in
  Fs.Directory.fold_files_rec_result ~ext:".odoc" add_deps () pkg_dir >>= fun () ->
  Ok (Hash_set.elements deps)
