open Sexp
open Comptypes

type compiler =
  { parent : compiler option
  ; used : StringSet.t
  ; forms : Srcloc.t sexp StringMap.t
  }

let rec compile_mod l opts args body =
  CompileError (opts.filename, l, "can't compile mod yet")

let rec compile_to_assembler opts pre_forms =
  match pre_forms with
  | [] ->
    CompileError
      (opts.filename, Srcloc.start, "empty source file not allowed")

  | [ ( Cons
          ( l
          , Atom (_,"mod")
          , Cons
              ( _
              , args
              , body
              )
          )
      )
    ] -> compile_mod l opts args body

  | (Cons (l, Atom (_,"mod"), _)) :: _ ->
    CompileError
      (opts.filename, l, "one toplevel mod form allowed")

  | hd :: tl ->
    let loc = location_of hd in
    compile_to_assembler opts
      [ Cons
          ( loc
          , Atom (loc,"mod")
          , Cons
              ( loc
              , Nil loc
              , list_to_cons location_of pre_forms
              )
          )
      ]

let compile_file opts content : string compileResult =
  let parse_result =
    parse_sexp
      Srcloc.combineSrcLocation
      Srcloc.start
      Srcloc.advance
      content
  in
  match parse_result with
  | Sexp.Failure (loc, err) -> CompileError (opts.filename, loc, err)
  | Sexp.Success pre_forms ->
    compile_to_assembler opts pre_forms
    |> compMap
      (fun result ->
         if opts.assemble then
           Sexp.encode result
         else
           Sexp.to_string result
      )
