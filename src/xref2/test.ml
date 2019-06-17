
(* When resolving paths, we go down the path until we find an Identifier. Once we've
   got that far we look up the component via the Identifier in the environment and return the
   resolved path for that Identifier and the Module component for it. As we then go up the Path.t
   we look up the Ident.t in the module component, finding a new module and passing
   that up the chain, building the resolved path as we go, until we pop out at the
   top to find the final thing - so far the only thing that can be handled is a
   type. *)


let mkenv () =
  Odoc_odoc.Env.create ~important_digests:false ~directories:[]

let resolve unit =
  let env = mkenv () in
  let resolve_env = Odoc_odoc.Env.build env (`Unit unit) in
  let resolver = Odoc_odoc.Env.resolver resolve_env in
  let result = Odoc_xref.resolve resolver unit in
  result

