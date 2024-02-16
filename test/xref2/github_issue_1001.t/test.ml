type t = int option

let rec f ?(optional : t) () = f ?optional ()
and g () = f ?optional:(Some 1) ()

