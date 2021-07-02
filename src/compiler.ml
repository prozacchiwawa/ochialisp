open Sexp
open Comptypes

(* Given a cons cell, rename occurrences of oldname to newname *)
let rec rename_in_cons namemap = function
  | Atom (l,name) ->
    begin
      try
        Atom (l,StringMap.find name namemap)
      with _ ->
        Atom (l,name)
    end
  | Cons (l,head,tail) ->
    Cons (l,rename_in_cons namemap head,rename_in_cons namemap tail)
  | any -> any

let argname_ctr = ref 0

let gensym name =
  let _ = argname_ctr := !argname_ctr + 1 in
  Printf.sprintf "%s_$_%d" name !argname_ctr

(* Returns a list of pairs containing the old and new atom names *)
let rec invent_new_names_sexp = function
  | Atom (_,name) -> [(name, gensym name)]
  | Cons (_,head,tail) ->
    List.concat [invent_new_names_sexp head; invent_new_names_sexp tail]
  | _ -> []

let make_binding_unique = function
  | Binding (l,name,body) -> (name, Binding (l, gensym name, body))

let rec rename_in_bodyform namemap = function
  | Let (l,bindings,body) ->
    let renames = List.map make_binding_unique bindings in
    let new_renamed_bindings = List.map snd renames in
    let local_namemap =
      renames
      |> List.map
        (function
          | (oldname, Binding (_, newname, _)) -> (oldname, newname)
        )
      |> StringMapBuilder.go
    in
    let locally_renamed_body = rename_in_bodyform local_namemap body in
    let new_bindings =
      List.map
        (function
          | Binding (l,name,body) ->
            Binding (l,name,rename_in_bodyform namemap body)
        )
        new_renamed_bindings
    in
    Let (l, new_bindings, rename_in_bodyform namemap locally_renamed_body)

  | Expr (l, e) ->
    Expr (l, rename_in_cons namemap e)

and rename_in_helperform namemap = function
  | Defconstant (l,n,body) ->
    Defconstant
      ( l
      , n
      , rename_in_cons namemap body
      )
  | Defmacro (l,n,arg,body) ->
    let new_names = invent_new_names_sexp arg in
    let local_namemap = new_names |> StringMapBuilder.go in
    let local_renamed_arg = rename_in_cons local_namemap arg in
    let local_renamed_body = rename_in_compileform local_namemap body in
    Defmacro
      ( l
      , n
      , local_renamed_arg
      , rename_in_compileform namemap local_renamed_body
      )
  | Defun (l,n,inline,arg,body) ->
    let new_names = invent_new_names_sexp arg in
    let local_namemap = new_names |> StringMapBuilder.go in
    let local_renamed_arg = rename_in_cons local_namemap arg in
    let local_renamed_body = rename_in_bodyform local_namemap body in
    Defun
      ( l
      , n
      , inline
      , local_renamed_arg
      , rename_in_bodyform namemap local_renamed_body
      )
  | any -> any

and rename_in_compileform namemap = function
  | Mod (l,arg,helpers,body) ->
    let new_names = invent_new_names_sexp arg in
    let local_namemap = new_names |> StringMapBuilder.go in
    let local_renamed_arg = rename_in_cons local_namemap arg in
    let local_renamed_helpers =
      List.map (rename_in_helperform local_namemap) helpers
    in
    let local_renamed_body = rename_in_bodyform local_namemap body in
    Mod
      ( l
      , local_renamed_arg
      , List.map (rename_in_helperform namemap) local_renamed_helpers
      , rename_in_bodyform namemap local_renamed_body
      )

let rec compile_bodyform opts = function
  | Cons
      ( l
      , Atom ( _, "let" )
      , Cons
          ( _
          , _bindings
          , Cons
              ( _
              , _body
              , Nil _
              )
          )
      ) ->
    CompileError (opts.filename,l,"can't yet compile let")

  | Cons
      ( l
      , Atom (_, "let" )
      , _
      ) ->
    CompileError (opts.filename,l,"bad let form")

  | any -> CompileOk (Expr (location_of any,any))

and compile_defun opts l inline name args body =
  compile_bodyform opts body
  |> compMap (fun bf -> Defun (l, name, inline, args, bf))

and compile_helperform opts = function
  | Cons
      ( l
      , Atom (_, "defconstant")
      , Cons
          ( _
          , Atom (_,_name)
          , Cons
              ( _
              , _body
              , Nil _
              )
          )
      ) ->
    CompileError (opts.filename, l, "can't yet compile defconstant")

  | Cons
      ( l
      , Atom (_, "defmacro")
      , Cons
          ( _
          , Atom (_,_name)
          , Cons
              ( _
              , _args
              , _body
              )
          )
      ) ->
    CompileError (opts.filename, l, "can't yet compile defmacro")

  | Cons
      ( l
      , Atom (_, "defun")
      , Cons
          ( _
          , Atom (_,name)
          , Cons
              ( _
              , args
              , Cons
                  ( _
                  , body
                  , Nil _
                  )
              )
          )
      ) ->
    compile_defun opts l false name args body

  | Cons
      ( l
      , Atom (_, "defun-inline")
      , Cons
          ( _
          , Atom (_,name)
          , Cons
              ( _
              , args
              , Cons
                  ( _
                  , body
                  , Nil _
                  )
              )
          )
      ) -> compile_defun opts l true name args body

  | any ->
    compile_bodyform opts any
    |> compMap (fun f -> TopExpr (location_of any, f))

and compile_mod_ mc opts args = function
  | Nil l -> CompileError (opts.filename, l, "no expression at end of mod")

  | Cons (l,body,Nil _) ->
    begin
      match mc with
      | ModAccum (l,helpers) ->
        compile_bodyform opts body
        |> compMap (fun bf -> ModFinal (Mod (l,args,helpers [],bf)))

      | ModFinal _ -> CompileError (opts.filename, l, "too many expressions")
    end

  | Cons (l,form,rest) ->
    begin
      match mc with
      | ModAccum (l,helpers) ->
        compile_helperform opts form
        |> compBind
          (fun form ->
             compile_mod_ (ModAccum (l, fun r -> form :: (helpers r))) opts args rest
          )

      | ModFinal _ -> CompileError (opts.filename, l, "too many expressions")
    end

  | any ->
    CompileError
      ( opts.filename
      , location_of any
      , Printf.sprintf "inappropriate sexp %s" (to_string any)
      )

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
    ] -> compile_mod_ (ModAccum (l,identity)) opts args body

  | (Cons (l, Atom (_,"mod"), _)) :: _ ->
    CompileError
      (opts.filename, l, "one toplevel mod form allowed")

  | hd :: _tl ->
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
    |> compBind
      (function
        | ModAccum (loc, _) -> CompileError (opts.filename, loc, "mod must end on expression")
        | ModFinal m -> CompileOk m
      )
    |> compMap (rename_in_compileform StringMap.empty)
    |> compBind
      (function
        | Mod (l,args,helpers,expr) ->
          CompileError (opts.filename, l, to_string (compileform_to_sexp identity identity (Mod (l,args,helpers,expr))))
(*
         if opts.assemble then
           Sexp.encode result
         else
           Sexp.to_string result *)
      )
