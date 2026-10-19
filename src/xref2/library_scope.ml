type t = Scoped of string list | Any

let any = Any

let of_libraries = function [] -> Any | libs -> Scoped libs
