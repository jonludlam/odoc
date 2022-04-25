(* HashCons implementation *)

type 'a hashed = { v : 'a; key : int * string }

let counter = ref 0

let name = ref "predefined"

let set_name n = name := n

let mk x =
  incr counter;
  { v = x; key = (!counter, !name) }

module HT1 = Hashtbl.MakeSeeded (struct
  type t = int * string

  let equal : t -> t -> bool =
   fun (a, b) (c, d) -> Int.equal a c && String.equal b d

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

module HT2 = Hashtbl.MakeSeeded (struct
  type t = (int * string) * (int * string)

  let equal : t -> t -> bool =
   fun ((a, b), (c, d)) ((e, f), (g, h)) ->
    Int.equal a e && Int.equal c g && String.equal b f && String.equal d h

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

module HT3 = Hashtbl.MakeSeeded (struct
  type t = (int * string) * (int * string) * (int * string) option

  let equal : t -> t -> bool =
   fun ((a, b), (c, d), efopt) ((g, h), (i, j), klopt) ->
    Int.equal a g && Int.equal c i && String.equal b h && String.equal d j && (
      match (efopt, klopt) with
      | Some (e, f), Some (k, l) -> Int.equal e k && String.equal f l
      | None, None -> true
      | _, _ -> false
    )

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

module HT2str = Hashtbl.MakeSeeded (struct
  type t = (int * string) * string

  let equal : t -> t -> bool =
   fun ((a, b), c) ((e, f), g) ->
    Int.equal a e && String.equal b f && String.equal c g

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

module HT2optstr = Hashtbl.MakeSeeded (struct
  type t = (int * string) option * string

  let equal : t -> t -> bool =
   fun (x, c) (y, g) ->
    match (x, y) with
    | None, None -> String.equal c g
    | Some (a, b), Some (e, f) ->
        Int.equal a e && String.equal b f && String.equal c g
    | _ -> false

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

module HTStr = Hashtbl.MakeSeeded (struct
  type t = string

  let equal = String.equal

  let hash (i : int) (x : t) = Hashtbl.hash (i, x)
end)

let gen1 : ('a hashed -> 'b) -> 'a hashed -> 'b hashed =
 fun mk' ->
  let module M = HT1 in
  let tbl = M.create 4095 in
  fun x ->
    let key = x.key in
    if M.mem tbl key then M.find tbl key
    else
      let y = mk (mk' x) in
      M.add tbl key y;
      y

let gen2 : ('a hashed * 'b hashed -> 'c) -> 'a hashed * 'b hashed -> 'c hashed =
 fun mk' ->
  let module M = HT2 in
  let tbl = M.create 4095 in
  fun (x, y) ->
    let key = (x.key, y.key) in
    if M.mem tbl key then M.find tbl key
    else
      let y = mk (mk' (x, y)) in
      M.add tbl key y;
      y

      let gen3 : ('a hashed * 'b hashed * 'c hashed option -> 'd) -> 'a hashed * 'b hashed * 'c hashed option -> 'd hashed =
        fun mk' ->
         let module M = HT3 in
         let tbl = M.create 4095 in
         fun (x, y, zopt) ->
           let key = (x.key, y.key, match zopt with | Some z -> Some z.key | None -> None) in
           if M.mem tbl key then M.find tbl key
           else
             let y = mk (mk' (x, y, zopt)) in
             M.add tbl key y;
             y
       

let gen_named :
    ('a -> string) -> ('b hashed * 'a -> 'c) -> 'b hashed * 'a -> 'c hashed =
 fun str_of_name mk' ->
  let module M = HT2str in
  let tbl = M.create 4095 in
  fun (x, y) ->
    let key = (x.key, str_of_name y) in
    if M.mem tbl key then M.find tbl key
    else
      let z = mk (mk' (x, y)) in
      M.add tbl key z;
      z

let gen_named_opt_parent :
    ('a -> string) ->
    ('b hashed option * 'a -> 'c) ->
    'b hashed option * 'a ->
    'c hashed =
 fun str_of_name mk' ->
  let module M = HT2optstr in
  let tbl = M.create 4095 in
  fun (x, y) ->
    let key =
      ((match x with Some x -> Some x.key | None -> None), str_of_name y)
    in
    if M.mem tbl key then M.find tbl key
    else
      let z = mk (mk' (x, y)) in
      M.add tbl key z;
      z

let gen_str : (string -> 'c) -> string -> 'c hashed =
 fun mk' ->
  let module M = HTStr in
  let tbl = M.create 4095 in
  fun x ->
    let key = x in
    if M.mem tbl key then M.find tbl key
    else
      let z = mk (mk' x) in
      M.add tbl key z;
      z
