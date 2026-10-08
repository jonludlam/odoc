let prefixes : (string, unit) Hashtbl.t = Hashtbl.create 8

let register prefix = Hashtbl.replace prefixes prefix ()

let prefix name =
  match String.index_opt name '.' with
  | None -> name
  | Some i -> String.sub name 0 i

let is_custom name = Hashtbl.mem prefixes (prefix name)

(* The tags that the parser knows, in [src/parser/lexer.mll]. *)
let builtin =
  [
    "author";
    "before";
    "canonical";
    "children_order";
    "closed";
    "deprecated";
    "hidden";
    "inline";
    "open";
    "order_category";
    "param";
    "raise";
    "raises";
    "return";
    "returns";
    "see";
    "short_title";
    "since";
    "toc_status";
    "version";
  ]

(* As the lexer reads the name of a custom tag: letters, then letters or
   digits, each of which can follow a ['_'] or a ['-']. The lexer reads odoc's
   own tags first, so the letters cannot be the name of one: [see-also] is
   read as [@see]. *)
let is_valid_prefix p =
  let n = String.length p in
  let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let alnum c = letter c || (c >= '0' && c <= '9') in
  let rec rest i =
    i = n
    || (alnum p.[i] && rest (i + 1))
    || (p.[i] = '_' || p.[i] = '-')
       && i + 1 < n
       && alnum p.[i + 1]
       && rest (i + 2)
  in
  let rec letters i = if i < n && letter p.[i] then letters (i + 1) else i in
  n > 0
  && letter p.[0]
  && rest 1
  && not (List.mem (String.sub p 0 (letters 0)) builtin)
