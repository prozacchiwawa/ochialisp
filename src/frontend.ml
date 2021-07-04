open Sexp
open Comptypes
open Prims
open Preprocessor
open Rename

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
  | Quoted e -> []
  | Value (Atom (l,v)) -> [v]
  | Value _ -> []
  | Call (l,vs) -> List.concat @@ List.map collect_used_names_bodyForm vs

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

let rec qq_to_expression = function
  | Cons
      ( l
      , Atom (_, "q")
      , body
      )  -> CompileOk (Quoted body)

  | Cons
      ( l
      , Atom (_,"quote")
      , Cons
          ( _
          , body
          , Nil _
          )
      ) -> CompileOk (Quoted body)

  | Cons
      ( l
      , Atom (_, "unquote")
      , Cons
          ( _
          , body
          , Nil _
          )
      ) -> compile_bodyform body

  | Cons (l,f,r) ->
    qq_to_expression_list (Cons (l,f,r))

  | any -> CompileOk (Quoted any)

and qq_to_expression_list = function
  | Cons (l,f,r) ->
    qq_to_expression f
    |> compBind
      (fun f ->
         qq_to_expression_list r
         |> compMap (fun r -> Call (l,[Value (Atom (l,"c")); f; r]))
      )

  | Nil l -> CompileOk (Quoted (Nil l))

  | any -> CompileError (location_of any, "Bad list tail " ^ to_string any)

and args_to_expression_list = function
  | Nil l -> CompileOk []

  | Cons
      ( l
      , first
      , rest
      ) ->
    args_to_expression_list rest
    |> compBind
      (fun args ->
         compile_bodyform first
         |> compMap (fun f -> f :: args)
      )

  | any -> CompileError (location_of any, "Bad list tail " ^ to_string any)

and compile_bodyform = function
  | Cons
      ( l
      , Atom ( _, "let")
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
      , Atom (_, "let")
      , _
      ) ->
    CompileError (l,"bad let form")

  | Cons
      ( l
      , Atom (_, "q")
      , body
      ) ->
    CompileOk (Quoted body)

  | Cons
      ( l
      , Integer (_, "1")
      , body
      ) ->
    CompileOk (Quoted body)

  | Cons
      ( l
      , Atom (_, "quote")
      , Cons
          ( _
          , body
          , Nil _
          )
      ) ->
    CompileOk (Quoted body)

  | Cons
      ( l
      , Atom (_, "qq")
      , Cons
          ( _
          , body
          , Nil _
          )
      ) ->
    qq_to_expression body

  | Cons
      ( l
      , head
      , rest
      ) ->
    let _ = Js.log @@ "first " ^ to_string head in
    let _ = Js.log @@ "args_to_expresson_list " ^ to_string rest in
    args_to_expression_list rest
    |> compBind
      (fun args ->
         compile_bodyform head
         |> compMap (fun func -> Call (l, func :: args))
      )

  | any ->
    let _ = Js.log @@ "value " ^ to_string any in
    CompileOk (Value any)

and compile_defun l inline name args body =
  compile_bodyform body
  |> compMap (fun bf -> Defun (l, name, inline, args, bf))

and compile_defmacro opts l name args body =
  let program =
    Cons
      ( l
      , Atom (l, "mod")
      , Cons
          ( l
          , args
          , body
          )
      )
  in
  frontend opts [program]
  |> compMap (fun p -> Defmacro (l, name, args, p))

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
    CompileError (l, "can't yet compile defconstant")

  | Cons
      ( l
      , Atom (_, "defmacro")
      , Cons
          ( _
          , Atom (_,name)
          , Cons
              ( _
              , args
              , body
              )
          )
      ) ->
    compile_defmacro opts l name args body
    |> compMap (fun a -> Some a)

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
    |> compMap (fun a -> Some a)

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
    |> compMap (fun a -> Some a)

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
    let _ =
      Js.log @@ "compile_helperform (" ^ to_string form ^ " . " ^ to_string rest ^ ")"
    in
    compile_helperform opts form
    |> compBind
      (function
        | None ->
          CompileError (l, "only the last form can be an exprssion in mod")

        | Some form ->
          match mc with
          | ModAccum (l,helpers) ->
            CompileOk (ModAccum (l, fun r -> form :: (helpers r)))
          | ModFinal _ ->
            CompileError (l, "too many expressions")
      )
    |> compBind (fun ma -> compile_mod_ ma opts args rest)


  | any ->
    CompileError
      ( location_of any
      , Printf.sprintf "inappropriate sexp %s" (to_string any)
      )

and frontend_start opts pre_forms =
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
      (fun ls ->
         compile_mod_ (ModAccum (l,identity)) opts args (list_to_cons l location_of ls)
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

and frontend opts pre_forms =
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
