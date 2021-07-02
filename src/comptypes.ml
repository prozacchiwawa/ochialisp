open Sexp

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type 'a compileResult =
  | CompileOk of 'a
  | CompileError of string * Srcloc.t * string

let compMap (f : 'a -> 'b) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> CompileOk (f a)
  | CompileError (f,l,e) -> CompileError (f,l,e)

let compBind (f : 'a -> 'b compileResult) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> f a
  | CompileError (f,l,e) -> CompileError (f,l,e)

let identity a = a

module type MapLike = sig
  type key
  type 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val empty : 'a t
end

module MapBuilder(S : MapLike) = struct
  let go lst =
    List.fold_left
      (fun coll (k,v) -> S.add k v coll)
      S.empty
      lst
end

module StringMapBuilder = MapBuilder(StringMap)

type compilerOpts =
  { includeDirs : string list
  ; filename : string
  ; readNewFile : compilerOpts -> string -> string -> (string * string) compileResult
  ; assemble : bool
  }

let rec list_to_cons_ loc_of accum = function
  | [] -> accum (Nil Srcloc.start)
  | hd :: tl -> list_to_cons_ loc_of (fun t -> accum (Cons (loc_of hd, hd, t))) tl

let list_to_cons loc_of = list_to_cons_ loc_of identity

type 'body binding = Binding of (Srcloc.t * string * 'body bodyForm)

and 'body bodyForm
  = Let of (Srcloc.t * 'body binding list * 'body bodyForm)
  | Expr of (Srcloc.t * 'body)

and ('arg, 'body) helperForm
  = Defconstant of (Srcloc.t * string * 'body)
  | Defmacro of (Srcloc.t * string * 'arg * ('arg, 'body) compileForm)
  | Defun of (Srcloc.t * string * bool * 'arg * 'body bodyForm)
  | TopExpr of (Srcloc.t * 'body bodyForm)

and ('arg, 'body) compileForm
  = Mod of (Srcloc.t * 'arg * ('arg, 'body) helperForm list * 'body bodyForm)

type ('arg, 'body) modAccumulator
  = ModAccum of
    (Srcloc.t * (('arg, 'body) helperForm list -> ('arg, 'body) helperForm list))
  | ModFinal of (('arg, 'body) compileForm)

let rec binding_to_sexp (body_to_sexp : 'body -> Srcloc.t sexp) = function
  | Binding (loc,name,body) ->
    Cons
      ( loc
      , Atom (loc,name)
      , Cons (loc,bodyform_to_sexp body_to_sexp body, Nil loc)
      )

and loc_of_bodyform = function
  | Let (loc, _, _) -> loc
  | Expr (loc, _) -> loc

and loc_of_binding = function
  | Binding (loc, _, _) -> loc

and bodyform_to_sexp (body_to_sexp : 'body -> Srcloc.t sexp) :
  'body bodyForm -> Srcloc.t sexp = function
  | Let (loc, bindings, body) ->
    let binding_translator : 'body binding -> Srcloc.t sexp =
      binding_to_sexp body_to_sexp
    in
    let translated_bindings =
      List.map binding_translator bindings
    in
    let bindings_cons : Srcloc.t sexp =
      list_to_cons location_of translated_bindings
    in
    let translated_body = bodyform_to_sexp body_to_sexp body in
    Cons
      ( loc
      , Atom (loc,"let")
      , Cons
          ( loc
          , bindings_cons
          , Cons
              ( loc
              , translated_body
              , Nil loc
              )
          )
      )

  | Expr (_,body) -> body_to_sexp body

let loc_of_compileform = function
  | Mod (l,_,_,_) -> l

let rec helperform_to_sexp arg_to_sexp body_to_sexp = function
  | Defconstant (l,n,b) ->
    Cons
      ( l
      , Atom (l, "defconstant")
      , Cons
          ( l
          , Atom (l, n)
          , Cons
              ( l
              , body_to_sexp b
              , Nil l
              )
          )
      )
  | Defmacro (l,n,a,b) ->
    Cons
      ( l
      , Atom (l, "defmacro")
      , Cons
          ( l
          , Atom (l,n)
          , Cons
              ( l
              , arg_to_sexp a
              , compileform_to_sexp arg_to_sexp body_to_sexp b
              )
          )
      )
  | Defun (l,n,inline,a,b) ->
    Cons
      (l
      , Atom (l, if inline then "defun-inline" else "defun")
      , Cons
          ( l
          , Atom (l,n)
          , Cons
              ( l
              , arg_to_sexp a
              , bodyform_to_sexp body_to_sexp b
              )
          )
      )

  | TopExpr (_,e) -> bodyform_to_sexp body_to_sexp e

and compileform_to_sexp arg_to_sexp body_to_sexp = function
  | Mod (l, args, helpers, exp) ->
    let fcvt = helperform_to_sexp arg_to_sexp body_to_sexp in
    let bcvt = bodyform_to_sexp body_to_sexp exp in
    Cons
      ( l
      , Atom (l, "mod")
      , Cons
          ( l
          , arg_to_sexp args
          , list_to_cons location_of @@
            ( List.concat
                [ List.map fcvt helpers ; [ bcvt ] ]
            )
          )
      )
