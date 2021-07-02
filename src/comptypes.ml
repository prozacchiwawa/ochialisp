open Sexp

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type 'a compileResult =
  | CompileOk of 'a
  | CompileError of string * Srcloc.t * string

type compilerOpts =
  { includeDirs : string list
  ; filename : string
  ; readNewFile : compilerOpts -> string -> string -> (string * string) compileResult
  ; assemble : bool
  }

let rec list_to_cons_ loc_of accum = function
  | [] -> accum (Nil Srcloc.start)
  | hd :: tl ->
    list_to_cons_ loc_of (fun t -> Cons (loc_of hd, hd, accum t)) tl

let list_to_cons loc_of = list_to_cons_ loc_of (fun a -> a)

type ('arg, 'body) helperForm
  = Defconstant of 'body
  | Defmacro of ('arg * 'body)
  | Defun of (bool * 'arg * 'body)

let form_to_sexp loc_of arg_to_sexp body_to_sexp = function
  | Defconstant b ->
    Cons
      ( loc_of b
      , Atom (loc_of b, "defconstant")
      , Cons
          ( loc_of b
          , body_to_sexp b
          , Nil (loc_of b)
          )
      )
  | Defmacro (a,b) ->
    Cons
      (loc_of b
      , Atom (loc_of b, "defmacro")
      , Cons
          ( loc_of b
          , arg_to_sexp a
          , body_to_sexp b
          )
      )
  | Defun (inline,a,b) ->
    Cons
      (loc_of b
      , Atom (loc_of b, if inline then "defun-inline" else "defun")
      , Cons
          ( loc_of b
          , arg_to_sexp a
          , body_to_sexp b
          )
      )

type ('arg, 'body) compileForm
  = Mod of ('arg * ('arg, 'body) helperForm list * 'body)

let compile_to_sexp loc_of arg_to_sexp body_to_sexp = function
  | Mod (args, helpers, exp) ->
    let fcvt = form_to_sexp loc_of arg_to_sexp body_to_sexp in
    Cons
      ( Srcloc.start
      , Atom (Srcloc.start, "mod")
      , Cons
          ( Srcloc.start
          , arg_to_sexp args
          , list_to_cons loc_of @@
            ( List.concat
                [ List.map fcvt helpers ; [ exp ] ]
            )
          )
      )

let compMap (f : 'a -> 'b) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> CompileOk (f a)
  | CompileError (f,l,e) -> CompileError (f,l,e)

let compBind (f : 'a -> 'b compileResult) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> f a
  | CompileError (f,l,e) -> CompileError (f,l,e)
