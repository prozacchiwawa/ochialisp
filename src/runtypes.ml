open Sexp

type 'a runResult
  = RunError of (Srcloc.t * string)
  | RunExn of (Srcloc.t * Srcloc.t sexp)
  | RunOk of 'a

let run_to_string (cvt : 'a -> string) (r : 'a runResult) =
  match r with
  | RunOk v -> Printf.sprintf "%s" (cvt v)
  | RunExn (l,e) ->
    Printf.sprintf "%s: throw(x) %s\n" (Srcloc.toString l) (to_string e)
  | RunError (l,e) ->
    Printf.sprintf "%s: %s\n" (Srcloc.toString l) e
