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
  let helper_atom h = Atom (loc_of_helperform h, name_of_helperform h) in
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
    Atom (l, name_of_helperform @@ Array.get array 0)
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

let lookup_prim compiler l name =
  try
    CompileOk (StringMap.find name compiler.prims)
  with _ ->
    CompileError (l, "no such prim " ^ name)

let codegen_to_sexp opts compiler =
  let l = Srcloc.start opts.filename in
  with_heading l "codegen" @@
  list_to_cons
    l
    location_of
    [ with_heading l "prims" @@
      cons_of_string_map l identity compiler.prims
    ; with_heading l "macros" @@
      cons_of_string_map l identity compiler.macros
    ; with_heading l "defuns" @@
      cons_of_string_map l identity compiler.defuns
    ; with_heading l "to_process" @@
      list_to_cons l location_of
        (List.map (fun h -> Atom (l,name_of_helperform h)) compiler.to_process)
    ; with_heading l "env" (Cons (l,compiler.env,Nil l))
    ; with_heading l "final_expr" @@
      Cons (l,bodyform_to_sexp l identity compiler.final_expr,Nil l)
    ]

let rec get_callable opts compiler l atom =
  match atom with
  | Atom (l,name) ->
    let macro =
      try
        Some (StringMap.find name compiler.macros)
      with _ ->
        None
    in
    let defun =
      try
        Some (StringMap.find name compiler.defuns)
      with _ ->
        None
    in
    let prim =
      try
        Some (StringMap.find name compiler.prims)
      with _ ->
        None
    in
    begin
      match (macro, defun, prim, atom) with
      | (Some macro, _, _, _) -> CompileOk (CallMacro macro)
      | (_, Some defun, _, _) -> CompileOk (CallDefun defun)
      | (_, _, Some prim, _) -> CompileOk (CallPrim prim)
      | (_, _, _, Atom (_, "com")) -> CompileOk RunCompiler
      | _ -> CompileError (l, "no such callable '" ^ name ^ "'")
    end

  | Integer (l,v) -> CompileOk (CallPrim (Integer (l,v)))

  | any -> CompileError (location_of any, "can't call object " ^ to_string any)

and process_macro_call opts compiler l args code =
  let run_outcome = run code (Cons (l,compiler.env,args)) in
  match run_outcome with
  | RunExn (ml,x) ->
    CompileError
      ( l
      , Printf.sprintf
          "macro aborted at %s with %s"
          (Srcloc.toString ml)
          (to_string x)
      )

  | RunError (rl,e) ->
    CompileError
      ( l
      , Printf.sprintf
          "error executing macro: %s %s"
          (Srcloc.toString rl)
          e
      )

  | RunOk v ->
    Frontend.compile_bodyform v
    |> compBind (fun body -> generate_expr_code opts compiler body)

and generate_args_code opts compiler l : Srcloc.t sexp bodyForm list -> Srcloc.t sexp compileResult = function
  | [] -> CompileOk (Nil l)
  | hd :: tl ->
    generate_args_code opts compiler l tl
    |> compBind
      (fun t ->
         generate_expr_code opts compiler hd
         |> compMap
           (function
             | Code (_,h) -> Cons (l,h,t)
           )
      )

and process_defun_call opts compiler l args lookup =
  CompileError (l,"can't yet do defun call")

and get_call_name l = function
  | Value (Atom (l,name)) -> CompileOk (Atom (l,name))
  | Value (Integer (l,v)) -> CompileOk (Integer (l,v))
  | any -> CompileError (l, "not yet callable " ^ to_string (bodyform_to_sexp l identity any))

and generate_expr_code (opts : compilerOpts) compiler expr : compiledCode compileResult =
  match expr with
  | Let (l,_bindings,_expr) -> CompileError (l, "can't yet do let")
  | Quoted q ->
    let l = location_of q in
    CompileOk (Code (l, primquote l q))
  | Value (Atom (l,"@")) ->
    CompileOk (Code (l, Integer (l,"1")))
  | Value (Atom (l,v)) ->
    create_name_lookup compiler l v
    |> compMap (fun f -> Code (l, f))
  | Value x -> CompileOk (Code (location_of x, primquote (location_of x) x))
  | Call (l,[]) -> CompileError (l, "created a call with no forms")
  | Call (l,hd :: tl) ->
    generate_args_code opts compiler l tl
    |> compBind (fun args -> get_call_name l hd |> compMap (fun h -> (h,args)))
    |> compBind
      (fun (hd,args) ->
         get_callable opts compiler l hd
         |> compBind
           (function
             | CallMacro code ->
               process_macro_call opts compiler l args code

             | CallDefun lookup ->
               process_defun_call opts compiler l args lookup

             | CallPrim p -> CompileOk (Code (l, Cons (l,p,args)))

             | RunCompiler ->
               match args with
               | Cons (_,body,Nil _) ->
                 let opts =
                   { opts with
                     compiler = Some compiler
                   ; assemble = false
                   ; stdenv = false
                   }
                 in
                 let evalcode = run body compiler.env in
                 opts.compileProgram opts body
                 |> compMap
                   (fun code ->
                      let _ =
                        Js.log @@
                        Printf.sprintf "compiled code %s yielding %s"
                          (to_string body)
                          (to_string code)
                      in
                      Code (l,primquote l code)
                   )

               | any ->
                 CompileError
                   ( l
                   , Printf.sprintf
                       "wierdly formed compile request: %s" (to_string any)
                   )
           )
      )

and codegen_ opts compiler = function
  | Defconstant (loc, name, body) ->
    CompileError (Srcloc.start opts.filename, "can't process defconstant forms yet")

  | Defmacro (loc, name, args, body) ->
    let macroProgram =
      compileform_to_sexp identity identity body
    in
    let _ = Js.log @@ "compile macro " ^ name ^ " with " ^ to_string macroProgram in
    let opts =
      { opts with
        compiler = Some compiler
      ; assemble = false
      ; stdenv = false
      }
    in
    opts.compileProgram opts macroProgram
    |> compMap
      (fun code ->
         { compiler with
           macros = StringMap.add name code compiler.macros
         }
      )

  | Defun (loc, name, inline, args, body) ->
    CompileError (Srcloc.start opts.filename, "can't process defun forms yet")

and is_defun = function
  | Defun _ -> true
  | _ -> false

and empty_compiler l =
  { prims = Prims.prims |> StringMapBuilder.go

  ; macros = StringMap.empty

  ; defuns = StringMap.empty

  ; env = Nil l

  ; to_process = []

  ; final_expr = Quoted (Nil l)

  ; final_code = None
  }

and start_codegen opts = function
  | Mod (l,args,helpers,expr) ->
    let live_helpers = List.filter is_defun helpers in
    let use_compiler =
      match opts.compiler with
      | None -> empty_compiler l
      | Some c -> c
    in
    { use_compiler with
      env = compute_env_shape l args live_helpers
    ; to_process = helpers
    ; final_expr = expr
    }

and final_codegen (opts : compilerOpts) (compiler : (Srcloc.t sexp, Srcloc.t sexp) primaryCodegen) =
  generate_expr_code opts compiler compiler.final_expr
  |> compMap
    (function
      | Code (l,code) ->
       { compiler with final_code = Some (Code (l,code)) }
    )

and codegen opts cmod =
  let compiler = start_codegen opts cmod in
  let _ = Js.log @@ "compile " ^ to_string (codegen_to_sexp opts compiler) in
  let _ = Js.log @@ "on code " ^ to_string (compileform_to_sexp identity identity cmod) in
  List.fold_left
    (fun c f -> compBind (fun comp -> codegen_ opts comp f) c)
    (CompileOk compiler)
    compiler.to_process
  |> compBind (final_codegen opts)
  |> compBind
    (fun c ->
       match c.final_code with
       | None ->
         CompileError (Srcloc.start opts.filename, "Failed to generate code")
       | Some (Code (l,code)) ->
         CompileOk code
    )
