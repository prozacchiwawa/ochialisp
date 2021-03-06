open Sexp
open Comptypes
open Frontend
open Codegen

let compile_file opts content : string compileResult =
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
    frontend opts pre_forms
    |> compBind (codegen opts)
    |> compMap
      (fun result ->
        if opts.assemble then
          Sexp.encode result
        else
          Sexp.to_string result
      )
