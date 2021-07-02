open Sexp
open Comptypes

let process_include opts name =
  opts.readNewFile opts opts.filename name
  |> compBind
    (fun (_,content) ->
       let parse_result =
         parse_sexp
           Srcloc.combineSrcLocation
           (Srcloc.start opts.filename)
           Srcloc.advance
           content
       in
       match parse_result with
       | Sexp.Failure (loc, err) -> CompileError (loc, err)
       | Sexp.Success pre_forms ->
         CompileOk pre_forms
    )

(* Expand include inline in forms *)
let process_pp_form opts = function
  | Cons
      ( _
      , Atom (_,"include")
      , Cons
          ( _
          , Atom (_,name)
          , Nil _
          )
      ) -> process_include opts name

  | Cons
      ( _
      , Atom (_,"include")
      , Cons
          ( _
          , QuotedString (_,_,name)
          , Nil _
          )
      ) -> process_include opts name

  | Cons
      ( l
      , Atom (_,"include")
      , any
      ) ->
    CompileError
      ( l
      , Printf.sprintf "bad tail %s in include" (to_string any)
      )

  | any -> CompileOk [any]

let rec preprocess opts : Srcloc.t sexp -> Srcloc.t sexp list compileResult = function
  | Cons (_,head,Nil _) ->
    process_pp_form opts head

  | Cons (_,head,rest) ->
    process_pp_form opts head
    |> compBind
      (fun lst ->
         preprocess opts rest
         |> compMap (fun rs -> List.concat [lst;rs])
      )

  | any -> CompileOk [any]
