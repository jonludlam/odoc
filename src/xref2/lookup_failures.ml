let strict_mode = ref false

type t = string

let failure_acc = ref []

let add f = failure_acc := f :: !failure_acc

let catch_failures f =
  let prev = !failure_acc in
  failure_acc := [];
  let r = f () in
  let failures = !failure_acc in
  failure_acc := prev;
  r, List.rev failures

(** Report a lookup failure to the enclosing [catch_failures] call. *)
let report fmt = Format.kasprintf add fmt

(** Like [report] above but may raise the exception [exn] if strict mode is enabled *)
let report_important exn fmt =
  if !strict_mode then raise exn
  else Format.kasprintf add fmt

let pp = Format.pp_print_string

let pp_failures ppf fs =
  List.iter (Format.fprintf ppf "%a@\n" pp) fs

let shed_failures (r, failures) =
  ( match failures with
    | [] -> ()
    | _ :: _ ->
      Format.fprintf Format.err_formatter
        "The following lookup failures occurred:@\n%a%!" pp_failures failures
  );
  r
