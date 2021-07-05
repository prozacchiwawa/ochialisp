open Sexp
open Prims

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type 'a compileResult =
  | CompileOk of 'a
  | CompileError of Srcloc.t * string

let compMap (f : 'a -> 'b) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> CompileOk (f a)
  | CompileError (l,e) -> CompileError (l,e)

let compBind (f : 'a -> 'b compileResult) : 'a compileResult -> 'b compileResult = function
  | CompileOk a -> f a
  | CompileError (l,e) -> CompileError (l,e)

let identity a = a

let const v _ = v

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

type compiledCode = Code of Srcloc.t * Srcloc.t sexp

type ('arg, 'body) callable
  = CallMacro of Srcloc.t sexp (* Compiled program *)
  | CallDefun of Srcloc.t sexp (* Env lookup *)
  | CallPrim of Srcloc.t sexp
  | RunCompiler

let rec list_to_cons_ l loc_of accum = function
  | [] -> accum (Nil l)
  | hd :: tl -> list_to_cons_ l loc_of (fun t -> accum (Cons (loc_of hd, hd, t))) tl

(* Turn an ocaml list into an sexp list *)
let list_to_cons l loc_of = list_to_cons_ l loc_of identity

(* A binding for a let *)
type 'body binding = Binding of (Srcloc.t * string * 'body bodyForm)

(* An expression body or a let form *)
and 'body bodyForm
  = Let of (Srcloc.t * 'body binding list * 'body bodyForm)
  | Quoted of 'body
  | Value of 'body
  | Call of (Srcloc.t * 'body bodyForm list)

(* Frontend mod forms (toplevel forms in tranditional lisp) *)
and ('arg, 'body) helperForm
  = Defconstant of (Srcloc.t * string * 'body bodyForm)
  | Defmacro of (Srcloc.t * string * 'arg * ('arg, 'body) compileForm)
  | Defun of (Srcloc.t * string * bool * 'arg * 'body bodyForm)

(* Mod form *)
and ('arg, 'body) compileForm
  = Mod of (Srcloc.t * 'arg * ('arg, 'body) helperForm list * 'body bodyForm)

(* Code generation phase *)
type ('arg,'body) primaryCodegen =
  { prims : Srcloc.t sexp StringMap.t
  ; macros : Srcloc.t sexp StringMap.t
  ; defuns : Srcloc.t sexp StringMap.t
  ; env : Srcloc.t sexp
  ; to_process : ('arg,'body) helperForm list
  ; final_expr : 'body bodyForm
  ; final_code : compiledCode option
  }

type compilerOpts =
  { includeDirs : string list
  ; filename : string
  ; readNewFile : compilerOpts -> string -> string -> (string * string) compileResult
  ; compiler : (Srcloc.t sexp, Srcloc.t sexp) primaryCodegen option
  ; compileProgram : compilerOpts -> Srcloc.t sexp -> Srcloc.t sexp compileResult
  ; assemble : bool
  ; stdenv : bool
  }

(* Frontend uses this to accumulate frontend forms *)
type ('arg, 'body) modAccumulator
  = ModAccum of
    (Srcloc.t * (('arg, 'body) helperForm list -> ('arg, 'body) helperForm list))
  | ModFinal of (('arg, 'body) compileForm)

let rec binding_to_sexp l (body_to_sexp : 'body -> Srcloc.t sexp) = function
  | Binding (loc,name,body) ->
    Cons
      ( loc
      , Atom (loc,name)
      , Cons (loc,bodyform_to_sexp l body_to_sexp body, Nil loc)
      )

and loc_of_bodyform : Srcloc.t sexp bodyForm -> Srcloc.t = function
  | Let (loc, _, _) -> loc
  | Quoted a -> location_of a
  | Call (loc, _) -> loc
  | Value a -> location_of a

and loc_of_binding = function
  | Binding (loc, _, _) -> loc

and bodyform_to_sexp l (body_to_sexp : 'body -> Srcloc.t sexp) :
  'body bodyForm -> Srcloc.t sexp = function
  | Let (loc, bindings, body) ->
    let binding_translator : 'body binding -> Srcloc.t sexp =
      binding_to_sexp l body_to_sexp
    in
    let translated_bindings =
      List.map binding_translator bindings
    in
    let bindings_cons : Srcloc.t sexp =
      list_to_cons l location_of translated_bindings
    in
    let translated_body = bodyform_to_sexp l body_to_sexp body in
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

  | Quoted body -> primquote (location_of body) body

  | Value body -> body

  | Call (l,exprs) ->
    list_to_cons l location_of @@ List.map (bodyform_to_sexp l identity) exprs

let loc_of_compileform = function
  | Mod (l,_,_,_) -> l

let loc_of_helperform = function
  | Defconstant (l,_,_) -> l
  | Defmacro (l,_,_,_) -> l
  | Defun (l,_,_,_,_) -> l

let name_of_helperform = function
  | Defconstant (_,n,_) -> n
  | Defmacro (_,n,_,_) -> n
  | Defun (_,n,_,_,_) -> n

(* Serialize frontend forms to sexp for debugging *)
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
              , bodyform_to_sexp l body_to_sexp b
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
              , Cons
                  ( l
                  , compileform_to_sexp arg_to_sexp body_to_sexp b
                  , Nil l
                  )
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
              , Cons
                  ( l
                  , bodyform_to_sexp l body_to_sexp b
                  , Nil l
                  )
              )
          )
      )

and compileform_to_sexp arg_to_sexp body_to_sexp = function
  | Mod (l, args, helpers, exp) ->
    let fcvt = helperform_to_sexp arg_to_sexp body_to_sexp in
    let bcvt = bodyform_to_sexp l body_to_sexp exp in
    Cons
      ( l
      , Atom (l, "mod")
      , Cons
          ( l
          , arg_to_sexp args
          , list_to_cons l location_of @@
            ( List.concat
                [ List.map fcvt helpers ; [ bcvt ] ]
            )
          )
      )

let loc_of_code = function
  | Code (l,_) -> l

let with_heading l name body = Cons (l,Atom (l,name),body)

let cons_of_string_map l cvt_body map =
  StringMap.bindings map
  |> List.map
    (fun (k,v) -> Cons (l,QuotedString (l,'\"',k),Cons (l,cvt_body v,Nil l)))
  |> list_to_cons l location_of
