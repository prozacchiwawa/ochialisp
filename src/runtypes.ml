open Sexp

type 'a runResult
  = RunError of (Srcloc.t * string)
  | RunExn of (Srcloc.t * Srcloc.t sexp)
  | RunOk of 'a
