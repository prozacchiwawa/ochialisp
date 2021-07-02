open Sexp
open Comptypes
open Frontend

(* As in the python code, produce a pair whose (thanks richard)

   - car is the compiled code and
   - cdr is the argument from the mod definition

*)
let compute_code_shape l helpers =
  let helper_atom h = Atom (location_of_helper h, name_of_helper h) in
  let rec build_tree s e helper_array =
    if e - s == 1 then
      helper_atom @@ Array.get helper_array s
    else
      let mid = (e + s) / 2 in
      let car = build_tree s mid helper_array in
      let cdr = build_tree mid e helper_array in
      Cons (l, car, cdr)
  in
  let array = Array.of_list helpers in
  let alen = Array.length array in
  if alen == 0 then
    Nil l
  else if alen == 1 then
    Atom (l, name_of_helper @@ Array.get array 0)
  else
    build_tree 0 alen array

let compute_env_shape l args helpers =
  let car = compute_code_shape l helpers in
  let cdr = args in
  Cons (l, car, cdr)

(* XXX *)
let create_name_lookup l _name _env = Integer (l, "1")

(* XXX *)
let codegen_to_sexp _codegen =
  Cons (Srcloc.start, Atom (Srcloc.start, "codegen"), Nil Srcloc.start)

let codegen opts compiler =
  match compiler.to_process with
  | [] ->
    CompileError (Srcloc.start opts.filename, "can't produce final exp yet")

  | _hd :: _tl ->
    CompileError (Srcloc.start opts.filename, "can't process forms yet")

let start_codegen _opts = function
  | Mod (l,args,helpers,expr) ->
    { prims = Prims.prims |> StringMapBuilder.go

    ; finished_code = StringMap.empty

    ; finished_replacements = StringMap.empty

    ; to_process = helpers

    ; final_expr = expr

    ; environment_shape = compute_env_shape l args helpers
    }

let codegen opts cmod =
  let compiler = start_codegen opts cmod in
  let _ = Js.log @@ to_string @@ codegen_to_sexp compiler in
  codegen opts compiler
