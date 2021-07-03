open Prims
open Sexp
open Comptypes
open Frontend
open Clvm

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

let lookup_callable compiler l name =
  create_name_lookup compiler l name

let lookup_prim compiler l name =
  try
    CompileOk (StringMap.find name compiler.prims)
  with _ ->
    CompileError (l, "no such prim " ^ name)

let codegen_to_sexp _compiler =
  Cons (Srcloc.start, Atom (Srcloc.start, "codegen"), Nil Srcloc.start)

let rec process_macro_args opts compiler = function
  | Atom (l,n) ->
    create_name_lookup compiler l n
    |> compMap (fun f -> Code (l,Cons (l,f,Nil l)))

  | Nil l -> CompileOk (Code (l,Nil l))

  | Cons (l,f,Nil _) ->
    generate_bodyform_code opts compiler f
    |> compMap
      (function
        | Code (l,f) -> Code (l,Cons (l, f, (Nil l)))
      )

  | Cons (l,f,r) ->
    process_macro_args opts compiler r
    |> compBind
      (function
        | Code (_,r) ->
          let _ = Js.log @@ "arg tail is " ^ to_string r in
          generate_bodyform_code opts compiler f
          |> compMap
            (function
              | Code (l,f) ->
                Code (l, Cons (l, f, r))
            )
      )

  | any ->
    let l = location_of any in
    CompileOk (Code (l, primquote l any))

and lookup_macro compiler l name =
  try
    CompileOk (StringMap.find name compiler.macros)
  with _ ->
    CompileError (l, "could not find macro " ^ name)

and process_macro_call opts compiler l name args =
  process_macro_args opts compiler args
  |> compBind
    (function
      | Code (l,args) ->
        lookup_macro compiler l name
        |> compMap (fun code -> (code,args))
    )
  |> compBind
    (fun (code,args) ->
       let run_outcome = run code (Cons (l,compiler.env,args)) in
       match run_outcome with
       | RunExn (ml,x) ->
         let _ = Js.log @@ "runexn " ^ to_string x in
         CompileError
           ( l
           , Printf.sprintf
               "macro %s aborted at %s with %s"
               name
               (Srcloc.toString ml)
               (to_string x)
           )

       | RunError (rl,e) ->
         let _ = Js.log @@ "runerror " ^ e in
         CompileError
           ( l
           , Printf.sprintf
               "error executing macro %s: %s %s"
               name
               (Srcloc.toString rl)
               e
           )

       | RunOk v ->
         let _ = Js.log @@ "runok " ^ to_string v in
         generate_bodyform_code opts compiler v
    )

and process_code_call compiler l name args =
  lookup_callable compiler l name
  |> compMap (fun flookup -> Code (l, primapply l flookup args))

and eval_prim_arg opts compiler = function
  | Atom (l,name) ->
    create_name_lookup compiler l name
    |> compMap (fun c -> Code (l,c))
  | Cons (l,f,r) -> generate_bodyform_code opts compiler (Cons (l,f,r))
  | any ->
    let l = location_of any in
    CompileOk (Code (l, primquote l any))

and eval_prim_args opts compiler = function
  | Cons (l,f,r) ->
    eval_prim_arg opts compiler f
    |> compBind
      (fun f ->
         eval_prim_args opts compiler r
         |> compMap (fun r -> (f,r))
      )
    |> compMap
      (fun (Code (_,f),Code (_,r)) -> Code (l,Cons (l,f,r)))

  | Nil l -> CompileOk (Code (l, Nil l))

  | any -> CompileError (location_of any, "bad tail for calling prim")

and process_prim_call opts compiler l name args =
  eval_prim_args opts compiler args
  |> compBind
    (function
      | Code (l,args) ->
       lookup_prim compiler l name
       |> compMap (fun f -> Code (l, primop l f args))
    )

and generate_bodyform_code opts compiler = function
  | Atom (l,n) ->
    create_name_lookup compiler l n |> compMap (fun i -> Code (l,i))
  | Cons (l,Atom (_,name),rest) ->
    begin
      match process_macro_call opts compiler l name rest with
      | CompileOk code -> CompileOk code
      | _ ->
        match process_code_call compiler l name rest with
        | CompileOk code -> CompileOk code
        | _ -> process_prim_call opts compiler l name rest
    end
  | Cons (l,Integer (li,v),rest) ->
    CompileOk (Code (l, Cons (l, Integer (li,v), rest)))
  | any ->
    let l = location_of any in
    CompileOk (Code (l, primquote l any))

and generate_expr_code opts compiler = function
  | Let (l,_bindings,_expr) -> CompileError (l, "can't yet do let")
  | Expr (_,e) -> generate_bodyform_code opts compiler e

let rec macro_explode_alist = function
  | Cons (l,Integer (_,v),rest) ->
    primcons l (Integer (l,v)) (macro_explode_alist rest)
  | Cons (l,first,rest) ->
    primcons l (macro_explode first) (macro_explode_alist rest)
  | any -> primquote (location_of any) any

and macro_explode = function
  | Cons (l,Integer (_,"1"),r) -> primquote l (primquote l r)
  | Cons (l,Integer (_,v),r) ->
    primcons l (primquote l (Integer (l,v))) (macro_explode_alist r)
  | any -> primquote (location_of any) any

let codegen_ opts compiler = function
  | Defconstant (loc, name, body) ->
    CompileError (Srcloc.start opts.filename, "can't process defconstant forms yet")

  | Defmacro (loc, name, args, body) ->
    let macroProgram =
      compileform_to_sexp identity identity body
    in
    opts.compileProgram opts macroProgram
    |> compMap
      (fun code ->
         let _ = Js.log @@ "have code for macro " ^ name ^ " -> " ^ to_string code in
         let exploded = macro_explode code in
         let _ = Js.log @@ "exploded " ^ to_string exploded in
         { compiler with
           macros = StringMap.add name exploded compiler.macros
         }
      )

  | Defun (loc, name, inline, args, body) ->
    CompileError (Srcloc.start opts.filename, "can't process defun forms yet")

let is_defun = function
  | Defun _ -> true
  | _ -> false

let start_codegen _opts = function
  | Mod (l,args,helpers,expr) ->
    let live_helpers = List.filter is_defun helpers in
    { prims = Prims.prims |> StringMapBuilder.go

    ; env = compute_env_shape l args live_helpers

    ; macros = StringMap.empty

    ; finished_code = StringMap.empty

    ; finished_replacements = StringMap.empty

    ; to_process = helpers

    ; final_expr = expr

    ; final_code = None
    }

let show_macros compiler =
  Js.log @@ "macros " ^ String.concat ";" (List.map fst (StringMap.bindings compiler.macros))

let final_codegen opts compiler =
  let _ = Js.log @@ "generate final code for " ^ to_string (bodyform_to_sexp (loc_of_bodyform compiler.final_expr) identity compiler.final_expr) in
  generate_expr_code opts compiler compiler.final_expr
  |> compMap
    (function
      | Code (l,code) ->
        let _ = Js.log @@ "code was " ^ to_string code in
       { compiler with final_code = Some (Code (l,code)) }
    )

let codegen opts cmod =
  let compiler = start_codegen opts cmod in
  let _ = Js.log @@ to_string @@ codegen_to_sexp compiler in
  List.fold_left
    (fun c f -> compBind (fun comp -> codegen_ opts comp f) c)
    (CompileOk compiler)
    compiler.to_process
  |> compMap (fun c -> show_macros c ; c)
  |> compBind (final_codegen opts)
  |> compBind
    (fun c ->
       match c.final_code with
       | None ->
         CompileError (Srcloc.start opts.filename, "Failed to generate code")
       | Some (Code (l,code)) ->
         CompileOk code
    )
