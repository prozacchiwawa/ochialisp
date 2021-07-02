open Sexp
open Comptypes

let process_include opts l name =
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
         CompileOk
           [ Cons
               ( l
               , Atom (l,"_$_include")
               , Cons
                   ( l
                   , QuotedString (l,'\"',name)
                   , list_to_cons l location_of pre_forms
                   )
               )
           ]
    )

(* Expand include inline in forms *)
let process_pp_form opts = function
  | Cons
      ( l
      , Atom (_,"include")
      , Cons
          ( _
          , Atom (_,name)
          , Nil _
          )
      ) -> process_include opts l name

  | Cons
      ( l
      , Atom (_,"include")
      , Cons
          ( _
          , QuotedString (_,_,name)
          , Nil _
          )
      ) -> process_include opts l name

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

let rec expand_form_list l rest = function
  | [] -> Cons (l,rest,Nil l)
  | hd :: tl -> Cons (l,hd,expand_form_list l rest tl)

let rec preprocess opts = function
  | Nil l -> CompileOk (Nil l)
  | Cons (l,head,rest) ->
    process_pp_form opts head
    |> compBind
      (fun lst ->
         preprocess opts rest
         |> compMap (fun r -> expand_form_list l r lst)
      )
  | any -> CompileOk any
