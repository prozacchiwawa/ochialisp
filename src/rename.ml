open Gensym
open Sexp
open Comptypes

let rec rename_in_qq namemap = function
  | Cons (_l,Atom (_,"unquote"),Cons (_,body,Nil _)) ->
    let _ = Js.log @@ "resume renaming " ^ to_string body in
    rename_in_cons namemap body
  | Cons (l,x,y) -> Cons (l,rename_in_qq namemap x,rename_in_qq namemap y)
  | any -> any

(* Given a cons cell, rename occurrences of oldname to newname *)
and rename_in_cons namemap = function
  | Atom (l,name) ->
    begin
      try
        Atom (l,StringMap.find name namemap)
      with _ ->
        Atom (l,name)
    end
  | Cons (l,Atom (la,"q"),any) -> Cons (l,Atom (la,"q"),any)
  | Cons (l,Atom (la,"quote"),Cons (_,v,Nil _)) ->
    Cons (l,Atom (la,"q"),v)
  | Cons (_l,Atom (_la,"qq"),Cons (_,qqexpr,_)) ->
    let _ = Js.log @@ "qq " ^ to_string qqexpr in
    rename_in_qq namemap qqexpr
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
