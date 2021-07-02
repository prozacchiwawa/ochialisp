open Gensym
open Sexp
open Comptypes
open Preprocessor

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
      , rename_in_bodyform namemap body
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

let rec compile_bodyform = function
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
    CompileError (l,"can't yet compile let")

  | Cons
      ( l
      , Atom (_, "let" )
      , _
      ) ->
    CompileError (l,"bad let form")

  | any -> CompileOk (Expr (location_of any,any))

and compile_defun l inline name args body =
  compile_bodyform body
  |> compMap (fun bf -> Defun (l, name, inline, args, bf))

and compile_helperform_inc opts = function
  | Nil _l -> CompileOk []
  | Cons (l,head,rest) ->
    compile_helperform opts head
    |> compBind
      (function
        | None ->
          CompileError
            (l, "includes can't yet contain the final expr")

        | Some forms ->
          compile_helperform_inc opts rest
          |> compMap (fun r -> List.concat [forms;r])
      )
  | any -> CompileError (location_of any, "not a proper tail cons")

and compile_helperform opts = function
  | Cons
      ( _l
      , Atom (_, "_$_include")
      , Cons
          ( _
          , QuotedString (_,_,name)
          , forms
          )
      ) ->
    compile_helperform_inc { opts with filename = name } forms
    |> compMap (fun a -> Some a)

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
    CompileError (l, "can't yet compile defconstant")

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
    CompileError (l, "can't yet compile defmacro")

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
    compile_defun l false name args body
    |> compMap (fun a -> Some [a])

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
      ) ->
    compile_defun l true name args body
    |> compMap (fun a -> Some [a])

  | _ -> CompileOk None

and compile_mod_ mc opts args = function
  | Nil l -> CompileError (l, "no expression at end of mod")

  | Cons (l,body,Nil _) ->
    begin
      match mc with
      | ModAccum (l,helpers) ->
        compile_bodyform body
        |> compMap (fun bf -> ModFinal (Mod (l,args,helpers [],bf)))

      | ModFinal _ -> CompileError (l, "too many expressions")
    end

  | Cons (l,form,rest) ->
    compile_helperform opts form
    |> compBind
      (function
        | Some formlist ->
          List.fold_left
            (fun ma form ->
               ma
               |> compBind
                 (function
                   | ModAccum (l,helpers) ->
                     CompileOk (ModAccum (l, fun r -> form :: (helpers r)))
                   | ModFinal _ ->
                     CompileError (l, "too many expressions")
                 )
            )
            (CompileOk mc)
            formlist
          |> compBind (fun ma -> compile_mod_ ma opts args rest)

        | None ->
          CompileError
            (l, "only the last form can be an exprssion in mod")
      )

  | any ->
    CompileError
      ( location_of any
      , Printf.sprintf "inappropriate sexp %s" (to_string any)
      )

let rec frontend_start opts pre_forms =
  match pre_forms with
  | [] ->
    CompileError
      (Srcloc.start opts.filename, "empty source file not allowed")

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
    ] ->
    preprocess opts body
    |> compBind
      (fun b ->
         compile_mod_ (ModAccum (l,identity)) opts args b
      )

  | (Cons (l, Atom (_,"mod"), _)) :: _ ->
    CompileError
      (l, "one toplevel mod form allowed")

  | hd :: _tl ->
    let loc = location_of hd in
    frontend_start opts
      [ Cons
          ( loc
          , Atom (loc,"mod")
          , Cons
              ( loc
              , Nil loc
              , list_to_cons loc location_of pre_forms
              )
          )
      ]

let rec collect_used_names_sexp = function
  | Atom (_,name) -> [name]
  | Cons (_,head,tail) ->
    List.concat [collect_used_names_sexp head;collect_used_names_sexp tail]
  | _ -> []

let rec collect_used_names_binding = function
  | Binding (_,_,expr) -> collect_used_names_bodyForm expr

and collect_used_names_bodyForm = function
  | Let (_,bindings,expr) ->
    List.concat
      [ List.concat (List.map collect_used_names_binding bindings)
      ; collect_used_names_bodyForm expr
      ]
  | Expr (_,e) -> collect_used_names_sexp e

and collect_used_names_helperForm = function
  | Defconstant (_,_,value) -> collect_used_names_bodyForm value
  | Defmacro (_,_,_,body) -> collect_used_names_compileForm body
  | Defun (_,_,_,_,body) -> collect_used_names_bodyForm body

and collect_used_names_compileForm = function
  | Mod (_,_,helpers,expr) ->
    List.concat
      [ List.concat (List.map collect_used_names_helperForm helpers)
      ; collect_used_names_bodyForm expr
      ]

let name_of_helper = function
  | Defconstant (_,n,_) -> n
  | Defmacro (_,n,_,_) -> n
  | Defun (_,n,_,_,_) -> n

let location_of_helper = function
  | Defconstant (l,_,_) -> l
  | Defmacro (l,_,_,_) -> l
  | Defun (l,_,_,_,_) -> l

let rec calculate_live_helpers opts last_names names helper_map =
  if StringSet.cardinal last_names = StringSet.cardinal names then
    CompileOk names
  else
    let new_names = StringSet.diff last_names names in
    List.fold_left
      (fun already_found name ->
         already_found
         |> compBind
           (fun found ->
              try
                let new_helper = StringMap.find name helper_map in
                let even_newer_names =
                  collect_used_names_helperForm new_helper
                in
                CompileOk
                  (StringSet.union found (StringSet.of_list even_newer_names))
              with _ ->
                CompileError
                  ( Srcloc.start opts.filename
                  , Printf.sprintf "unbound name %s" name
                  )
           )
      )
      (CompileOk names)
      (StringSet.elements new_names)
    |> compBind
      (fun new_names -> calculate_live_helpers opts names new_names helper_map)

let frontend opts pre_forms =
  frontend_start opts pre_forms
  |> compBind
    (function
      | ModAccum (loc, _) -> CompileError (loc, "mod must end on expression")
      | ModFinal m -> CompileOk m
    )
  |> compMap (rename_in_compileform StringMap.empty)
  |> compBind
    (function
      | Mod (l,args,helpers,expr) ->
        let expr_names =
          StringSet.of_list @@ collect_used_names_bodyForm expr
        in
        let helper_map =
          helpers
          |> List.map (fun h -> (name_of_helper h, h))
          |> StringMapBuilder.go
        in
        calculate_live_helpers opts StringSet.empty expr_names helper_map
        |> compMap
          (fun helper_names ->
             let live_helpers =
               List.filter
                 (fun h -> StringSet.mem (name_of_helper h) helper_names)
                 helpers
             in
             Mod (l,args,live_helpers,expr)
          )
    )
