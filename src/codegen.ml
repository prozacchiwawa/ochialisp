open Prims
open Sexp
open Comptypes
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

let rec get_callable _opts compiler _l atom =
  match atom with
  | Atom (l,name) ->
    let macro =
      try
        Some (StringMap.find name compiler.macros)
      with _ ->
        None
    in
    let defun =
      create_name_lookup compiler l name
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
      | (_, CompileOk defun, _, _) -> CompileOk (CallDefun defun)
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

and process_defun_call _opts _compiler l args lookup =
  let rec cons_up = function
    | Cons (l,h,r) -> primcons l h (cons_up r)
    | any -> any
  in
  let env = primcons l (Integer (l,"2")) (cons_up args) in
  CompileOk
    (Code
       ( l
       , primapply l lookup env
       )
    )


(* (Code (l, Cons (l, Atom (l,"q"), env))) *)

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
                 opts.compileProgram opts body
                 |> compMap
                   (fun code ->
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

and combine_defun_env old_env new_args =
  match old_env with
  | Cons (l,h,_) -> Cons (l,h,new_args)
  | any -> any

and codegen_ opts compiler = function
  | Defconstant (_loc, _name, _body) ->
    CompileError (Srcloc.start opts.filename, "can't process defconstant forms yet")

  | Defmacro (_loc, name, _args, body) ->
    let macroProgram =
      compileform_to_sexp identity identity body
    in
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

  | Defun (loc, name, _inline, args, body) ->
    let opts =
      { opts with
        compiler =
          Some { compiler with parentfns = StringSet.add name compiler.parentfns }
      ; inDefun = true
      ; assemble = false
      ; stdenv = false
      ; startEnv = Some (combine_defun_env compiler.env args)
      }
    in
    let tocompile =
      Cons
        ( loc
        , Atom (loc,"mod")
        , Cons
            ( loc
            , args
            , Cons
                ( loc
                , bodyform_to_sexp loc identity body
                , Nil loc
                )
            )
        )
    in
    let _ =
      Js.log @@
      Printf.sprintf "raw input %s is %s"
        name
        (to_string tocompile)
    in
    opts.compileProgram opts tocompile
    |> compMap
      (fun code ->
         let _ =
           Js.log @@
           Printf.sprintf "compiled %s to %s"
             name
             (to_string code)
         in
         { compiler with
           defuns = StringMap.add name code compiler.defuns
         }
      )

and is_defun = function
  | Defun _ -> true
  | _ -> false

and empty_compiler l =
  { prims = Prims.prims |> StringMapBuilder.go

  ; macros = StringMap.empty

  ; defuns = StringMap.empty

  ; parentfns = StringSet.empty

  ; env = Cons (l, Nil l, Nil l)

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
      env =
        begin
          match opts.startEnv with
          | Some env -> env
          | None -> compute_env_shape l args live_helpers
        end
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

and finalize_env_ opts c _l env =
  match env with
  | Atom (l,v) ->
    (* Parentfns are functions in progress in the parent *)
    if StringSet.mem v c.parentfns then
      CompileOk (Nil l)
    else
      begin
        try
          CompileOk (StringMap.find v c.defuns)
        with _ ->
          CompileError
            (l, "A defun was referenced in the defun env but not found " ^ v)
      end

  | Cons (l,h,r) ->
    finalize_env_ opts c l h
    |> compBind (fun h -> finalize_env_ opts c l r |> compMap (fun r -> Cons (l,h,r)))

  | any -> CompileOk any

and finalize_env opts c _l =
  match c.env with
  | Cons (l,h,_r) -> finalize_env_ opts c l h
  | any -> CompileOk any

and codegen opts cmod =
  let compiler = start_codegen opts cmod in
  List.fold_left
    (fun c f -> compBind (fun comp -> codegen_ opts comp f) c)
    (CompileOk compiler)
    compiler.to_process
  |> compBind (final_codegen opts)
  |> compBind
    (fun c ->
       let l = loc_of_compileform cmod in
       finalize_env opts c l
       |> compBind
         (fun final_env ->
            match c.final_code with
            | None ->
              CompileError (Srcloc.start opts.filename, "Failed to generate code")
            | Some (Code (l,code)) ->
              if opts.inDefun || nilp final_env then
                CompileOk code
              else
                CompileOk
                  (primapply
                     l
                     (primquote l code)
                     (primcons
                        l
                        (primquote l final_env)
                        (Atom (l,"1"))
                     )
                  )
         )
    )
