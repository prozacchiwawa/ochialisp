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

let rec create_name_lookup_ l name env = function
  | Atom (l,a) ->
    if a == name then
      CompileOk 1
    else
      CompileError (l, Printf.sprintf "%s not found (via %s)" name a)
  | Cons (l,head,rest) ->
    begin
      match create_name_lookup_ l name env head with
      | CompileError _ ->
        create_name_lookup_ l name env rest |> compMap (fun v -> 2 * v + 1)
      | CompileOk v -> CompileOk (2 * v)
    end
  | any ->
    CompileError
      ( l
      , Printf.sprintf
          "%s not found checking %s in %s"
          name (to_string any) (to_string env)
      )

let create_name_lookup compiler l name =
  create_name_lookup_ l name compiler.env compiler.env
  |> compMap (fun i -> Integer (l,string_of_int i))

let codegen_to_sexp _compiler =
  Cons (Srcloc.start, Atom (Srcloc.start, "codegen"), Nil Srcloc.start)

let rec process_apply_args compiler = function
  | Atom (l,n) ->
    create_name_lookup compiler l n
    |> compMap (fun f -> Code (l,f))
  | Cons (_,f,r) ->
    process_apply_args compiler r
    |> compBind
      (function
        | Code (_,r) ->
          generate_bodyform_code compiler f
          |> compMap
            (function
              | Code (l,f) -> Code (l, Cons (l, Integer (l,"4"), Cons (l,f,r)))
            )
      )

  | any -> CompileOk (Code (location_of any, any))

and generate_bodyform_code compiler = function
  | Atom (l,n) ->
    create_name_lookup compiler l n |> compMap (fun i -> Code (l,i))
  | Cons (_,Atom (_,name),rest) ->
    process_apply_args compiler rest
    |> compBind
      (function
        | Code (l,args) ->
          create_name_lookup compiler l name
          |> compMap
            (fun flookup ->
                Code
                  ( l
                  , Cons
                      ( l
                      , Integer (l,"2")
                      , Cons
                          ( l
                          , flookup
                          , Cons (l,args,Nil l)
                          )
                      )
                  )
            )
      )
  | any -> CompileOk (Code (location_of any,any))

and generate_expr_code compiler = function
  | Let (l,_bindings,_expr) -> CompileError (l, "can't yet do let")
  | Expr (_,e) -> generate_bodyform_code compiler e

let codegen opts compiler =
  match compiler.to_process with
  | [] -> generate_expr_code compiler compiler.final_expr

  | _hd :: _tl ->
    CompileError (Srcloc.start opts.filename, "can't process forms yet")

let start_codegen _opts = function
  | Mod (l,args,helpers,expr) ->
    { prims = Prims.prims |> StringMapBuilder.go

    ; env = compute_env_shape l args helpers

    ; macros = StringMap.empty

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
