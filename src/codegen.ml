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
  try
    CompileOk (StringMap.find name compiler.constants)
  with _ ->
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
      cons_of_string_map l
        (fun dc ->
           Cons (l,dc.requiredEnv, Cons (l,dc.code,Nil l))
        )
        compiler.defuns
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
  let converted_args = List.map (bodyform_to_sexp l identity) args in
  let args_to_macro = list_to_cons l location_of converted_args in
  let run_outcome = run code args_to_macro in
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
  (* CompileOk (Code (location_of x, primquote (location_of x) x))*)
  | Value x ->
    CompileOk (Code (location_of x, primquote (location_of x) x))
  | Call (l,[]) -> CompileError (l, "created a call with no forms")
  | Call (l,Value (Atom (al,an)) :: tl) ->
    get_callable opts compiler l (Atom (al,an))
    |> compBind
      (function
        | CallMacro code ->
          process_macro_call opts compiler l tl code

        | CallDefun lookup ->
          generate_args_code opts compiler l tl
          |> compBind
            (fun args ->
               process_defun_call opts compiler l args lookup
            )

        | CallPrim p ->
          generate_args_code opts compiler l tl
          |> compBind
            (fun args ->
               CompileOk (Code (l, Cons (l,p,args)))
            )

        | RunCompiler ->
          match tl with
          | [compile] ->
            let opts =
              { opts with
                compiler = Some compiler
              ; assemble = false
              ; stdenv = false
              ; inDefun = true
              }
            in
            let use_body =
              Cons
                ( l
                , Atom (l, "mod")
                , Cons
                    ( l
                    , Nil l
                    , Cons
                        ( l
                        , bodyform_to_sexp l identity compile
                        , Nil l
                        )
                    )
                )
            in
            opts.compileProgram opts use_body
            |> compMap
              (fun code ->
                 Code (l, primquote l code)
              )

          | any ->
            CompileError
              ( l
              , Printf.sprintf
                  "wierdly formed compile request: %s"
                  (String.concat ";" @@ List.map to_string @@ List.map (bodyform_to_sexp l identity) any)
              )
      )
  | ex ->
    CompileError
      ( loc_of_bodyform ex
      , "don't know how to compile " ^
        to_string (bodyform_to_sexp (loc_of_bodyform ex) identity ex)
      )

and combine_defun_env old_env new_args =
  match old_env with
  | Cons (l,h,_) -> Cons (l,h,new_args)
  | any -> any

and codegen_ opts compiler = function
  | Defconstant (loc, name, body) ->
    let expandProgram =
      compileform_to_sexp identity identity (Mod (loc, Nil loc, [], body))
    in
    let opts =
      { opts with
        compiler = Some compiler
      }
    in
    opts.compileProgram opts expandProgram
    |> compBind
      (fun code ->
         match run code (Nil loc) with
         | RunOk res -> CompileOk res
         | r ->
           CompileError
             ( loc
             , Printf.sprintf
                 "Error evaluating constant: %s"
                 (Runtypes.run_to_string to_string r)
             )
      )
    |> compMap
      (fun res ->
         { compiler with
           constants =
             StringMap.add name (primquote loc res) compiler.constants
         }
      )

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
        compiler = Some compiler
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
    opts.compileProgram opts tocompile
    |> compMap
      (fun code ->
         { compiler with
           defuns =
             StringMap.add
               name
               { requiredEnv = args
               ; code = code
               }
               compiler.defuns
         }
      )

and is_defun = function
  | Defun _ -> true
  | _ -> false

and empty_compiler l =
  { prims = Prims.prims |> StringMapBuilder.go

  ; constants = StringMap.empty

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
    begin
      try
        CompileOk (StringMap.find v c.defuns).code
      with _ ->
        (* Parentfns are functions in progress in the parent *)
        if StringSet.mem v c.parentfns then
          CompileOk (Nil l)
        else
          CompileError
            (l, "A defun was referenced in the defun env but not found " ^ v)
    end

  | Cons (l,h,r) ->
    finalize_env_ opts c l h
    |> compBind
      (fun h ->
         finalize_env_ opts c l r |> compMap (fun r -> Cons (l,h,r))
      )

  | any -> CompileOk any

and finalize_env opts c =
  match c.env with
  | Cons (l,h,_) -> finalize_env_ opts c l h
  | any -> CompileOk any

and dummy_functions compiler =
  List.fold_left
    (fun c -> function
       | Defconstant _ -> c
       | Defmacro _ -> c
       | Defun (_,name,_,_,_) ->
         { c with parentfns = StringSet.add name c.parentfns }
    )
    compiler
    compiler.to_process

and codegen opts cmod =
  let compiler =
    start_codegen opts cmod
    |> dummy_functions
  in
  List.fold_left
    (fun c f -> c |> compBind (fun comp -> codegen_ opts comp f))
    (CompileOk compiler)
    compiler.to_process
  |> compBind
    (fun c ->
       finalize_env opts c
       |> compBind
         (fun final_env ->
            match c.final_code with
            | None ->
              CompileError (Srcloc.start opts.filename, "Failed to generate code")
            | Some (Code (l,code)) ->
              if opts.inDefun then
                let final_code =
                  (primapply
                     l
                     (primquote l code)
                     (Integer (l,"1"))
                  )
                in
                CompileOk final_code
              else
                let final_code =
                  (primapply
                     l
                     (primquote l code)
                     (primcons
                        l
                        (primquote l final_env)
                        (Integer (l,"1"))
                     )
                  )
                in
                CompileOk final_code
         )
    )
