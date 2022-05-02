(* HashCons implementation *)

type 'a hashed = { v : 'a }

let gen1 f x = { v = f x }

let mk x = { v = x }

let gen_str f x = { v = f x }

let gen_named _ f x = { v = f x }

let gen2 f x = { v = f x }

let gen2canonical_m f x = { v = f x }

let gen3 f x = { v = f x }

let gen2canonical_mt f x = { v = f x }

let gen2canonical_t f x = { v = f x }
