open Sexp

type 'a compileResult =
  | CompileOk of 'a
  | CompileError of Srcloc.t * string

type compilerOpts =
  { includeDirs : string list
  ; filename : string
  ; readNewFile : compilerOpts -> string -> string -> (string * string) compileResult
  }

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbolLookupResult
  = FunctionArg of Srcloc.t * int
  | Constant of Srcloc.t sexp
  | Macro of Srcloc.t sexp
  | FunDef of Srcloc.t sexp

type compiler =
  { parent : compiler option
  ; used : StringSet.t
  ; symbols : symbolLookupResult StringMap.t
  }

let empty =
  { parent = None
  ; used = StringSet.empty
  ; symbols = StringMap.empty
  }

type 'c compileFormResult =
  | FinalForm of 'c * Srcloc.t sexp
  | FormError of Srcloc.t * string
  | UpdateCompiler of 'c

let fst (a,_) = a
let snd (_,b) = b

let cons_length a b =
  let counted = ref 0 in
  let current = ref (a,b) in
  let _ =
    while not @@ nilp @@ snd !current do
      let _ = counted := !counted + 1 in
      match b with
      | Cons (_,x,y) -> current := (x,y)
      | _ -> current := (Nil Srcloc.start,Nil Srcloc.start)
    done
  in
  !counted

let lookup_symbol compiler name =
  try
    Some (StringMap.find name compiler.symbols)
  with _ ->
    None

let compiler_result_to_update = function
  | CompileOk c -> UpdateCompiler c
  | CompileError (l,e) -> FormError (l,e)

let compiler_result_to_form = function
  | CompileOk (u,r) -> FinalForm (u,r)
  | CompileError (l,e) -> FormError (l,e)

(* reason on js doesn't do tail recursion, so do this with an array
 * as an intermediate.
 *)
let body_cons_to_list = function
  | Cons (l,a,b) ->
    let llen = cons_length a b in
    let resarray = Array.make llen (Nil l) in
    let ccons = ref (a,b) in
    let idx = ref 0 in
    let _ =
      while not @@ nilp @@ snd !ccons do
        let _ = Array.set resarray !idx @@ fst !ccons in
        let _ = idx := !idx + 1 in
        match snd !ccons with
        | Cons (_,x,y) -> ccons := (x,y)
        | _ -> ccons := (Nil Srcloc.start,Nil Srcloc.start)
      done
    in
    Array.to_list resarray
  | Nil _ -> []
  | item -> [item] (* Anything else is treated as a toplevel form *)

let list_to_cons = function
  | [] -> Nil Srcloc.start
  | x ->
    let resarray = Array.of_list x in
    let llen = Array.length resarray in
    let conses = ref (Nil Srcloc.start) in
    let _ =
      for i = 1 to llen do
        conses := Cons (Srcloc.start, Array.get resarray (llen - i), !conses)
      done
    in
    !conses

let rec strip_useless_sexp =
  function
  | Cons (_, Comment _, tl) -> strip_useless_sexp tl
  | Cons (_, EmptyLine _, tl) -> strip_useless_sexp tl
  | Cons (l, allowed, tl) -> Cons (l, allowed, strip_useless_sexp tl)
  | form -> form

let is_useless = function
  | Comment _ -> true
  | EmptyLine _ -> true
  | _ -> false

let strip_useless pre_forms =
  List.map
    strip_useless_sexp
    (List.filter (fun form -> not (is_useless form)) pre_forms)

let compMap f = function
  | CompileOk r -> CompileOk (f r)
  | CompileError (l,e) -> CompileError (l,e)

let compBind f = function
  | CompileOk r -> f r
  | CompileError (l,e) -> CompileError (l,e)

let formBind (f : 'c -> 'd compileFormResult) = function
  | UpdateCompiler c -> f c
  | FormError (l,e) -> FormError (l,e)
  | FinalForm (_,f) -> FormError (location_of f, "A nonbinding final form was already processed")

let compile_defun compiler l args body : compiler compileFormResult =
  FormError (l, "defun not implemented yet")

let compile_macro compiler l args body : compiler compileFormResult =
  FormError (l, "defmacro not implemented yet")

let compile_inline compiler l args body : compiler compileFormResult =
  FormError (l, "defun-inline not implemented yet")

let compile_expand_macro compiler l s args : compiler compileFormResult =
  FormError (l, "can't expand macro yet")

let compile_call_function compiler l body args : (compiler * Srcloc.t sexp) compileResult =
  CompileError (l, "can't call function yet")

let eval compiler l arg =
  CompileError (l, "can't eval yet")

let compile_top_eval compiler l name args : compiler compileFormResult =
  let to_invoke = lookup_symbol compiler name in
  let evaluated_args =
    List.fold_left
      (fun alist arg ->
         alist
         |> compBind
           (fun atl ->
              eval compiler l arg |> compMap (fun n -> n :: atl)
           )
      )
      (CompileOk [])
      args
  in
  let eval_function arglist : compiler compileFormResult =
    match to_invoke with
    | None -> FormError (l, "no symbol " ^ name ^ " at toplevel context")
    | Some (Macro s) ->
      compile_expand_macro compiler l s arglist
    | Some (FunDef body) ->
      compile_call_function compiler l body arglist
      |> compiler_result_to_form
    | Some (Constant _) ->
      FormError (l, "cannot invoke constant " ^ name ^ " as a toplevel form")
    | Some (FunctionArg (loc,id)) ->
      compile_call_function compiler l (Cons (l, Atom (loc, "a"), Integer (loc, string_of_int id))) (list_to_cons arglist)
      |> compiler_result_to_form
  in
  match evaluated_args with
  | CompileError (l,e) -> FormError (l,e)
  | CompileOk args -> eval_function args

let rec include_file compiler opts incfile : compiler compileFormResult =
  opts.readNewFile opts opts.filename incfile
  |> compiler_result_to_update
  |> formBind
    (fun (new_file_name, new_file_content) ->
      let parse_result =
        parse_sexp
          Srcloc.combineSrcLocation
          Srcloc.start
          Srcloc.advance
          new_file_content
      in
      match parse_result with
      | Sexp.Failure (loc, err) -> FormError (loc, err)
      | Sexp.Success pre_forms ->
        List.fold_left
          (fun compiler form ->
             compiler
             |> formBind
               (fun compiler ->
                  compile_mod_form
                    compiler
                    { opts with filename = new_file_name }
                    form
               )
          )
          (UpdateCompiler compiler)
          pre_forms
    )

and compile_mod_form compiler opts : Srcloc.t sexp -> compiler compileFormResult =
  function
  | QuotedString (l,ch,q) ->
    FinalForm (compiler, Cons (l, Atom (l,"q"), QuotedString (l,ch,q)))
  | Integer (l, v) ->
    FinalForm (compiler, Cons (l, Atom (l,"q"), Atom (l,v)))

  | Atom (l, v) ->
    begin
      match lookup_symbol compiler v with
      | None -> FormError (l, "unknown name in context " ^ v)
      | Some (FunctionArg (_, n)) ->
        (* No need for arg rewriting since it's a toplevel form *)
        FinalForm (compiler, Atom (l, string_of_int n))
      | Some (Constant value) ->
        (* Constant as a toplevel form just quotes the value *)
        FinalForm (compiler, Cons (l, Atom (l, "q"), value))
      | Some (Macro _) ->
        (* Can't use a bare macro name as a toplevel form *)
        FormError (l, v ^ " names a macro and can't be used as an atom used as a toplevel form")
      | Some (FunDef f) ->
        (* In lisp, a function has no value as an atom *)
        FormError (l, v ^ " lisp doesn't define the value of a defun'd atom")
    end
  | Cons (l, Atom (_,"include"), Cons (_, Atom (_, incname), _)) ->
    include_file compiler opts incname
  | Cons (l, Atom (_,"defun"), Cons (_, Atom (_, name), Cons (_, Atom (_, args), body))) ->
    compile_defun compiler l args body
  | Cons (l, Atom (_,"defmacro"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_macro compiler l args body
  | Cons (l, Atom (_,"defun-inline"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_inline compiler l args body
  | Cons (l, Atom (_,name), args) ->
    compile_top_eval compiler l name (body_cons_to_list args)
  | Cons (l, _, _) ->
    FormError (l, "malformed toplevel form")
  | Nil l -> FinalForm ({ compiler with used = StringSet.empty }, Nil l)
  | Comment (_,_) -> UpdateCompiler compiler
  | EmptyLine _ -> UpdateCompiler compiler

let rec make_arg_list d n (a,b) : (string * int) list compileResult =
  let addend_atom side = if side then d / 2 else 0 in
  let addend_cons side = if side then d else 0 in
  let make_side (side : bool) = function
    | Atom (_,name) -> CompileOk [(name, d + n + addend_atom side)]
    | Cons (_,x,y) -> make_arg_list (2 * d) (n + addend_cons side) (x,y)
    | Nil l -> CompileError (l, "nil doesn't make sense in a param list")
    | Integer (l,v) -> CompileError (l, "int doesn't make sense in a param list")
    | QuotedString (l,_,_) -> CompileError (l, "quoted string doesn't make sense in a param list")
    | Comment (l,_) ->
      CompileError (l,"unexpected comment in arglist (should have been stripped")
    | EmptyLine l ->
      CompileError (l,"unexpected emptyline in arglist (should have been stripped")
  in
  let left_side = make_side false a in
  let right_side = make_side true b in
  left_side
  |> compBind (fun l -> right_side |> compMap (fun r -> (l,r)))
  |> compMap (fun (l,r) -> List.concat [l;r])

let compile_mod compiler opts args body =
  let mod_args =
    match args with
    | Nil _ -> CompileOk []
    | Cons (_,a,b) -> make_arg_list 2 0 (a,b)
    | Atom (l,v) -> CompileOk [(v, 1)]
    | QuotedString (l,_,_) -> CompileError (l,"quoted string given as argument list for mod")
    | Integer (l,_) -> CompileError (l,"integer given as argument list for mod")
    | x -> CompileError (Srcloc.start, "unexpected token as mod argument list")
  in
  let final_result =
    List.fold_left
      (fun compiler form ->
         compiler
         |> formBind
           (fun c -> compile_mod_form c opts form)
      )
      (UpdateCompiler compiler)
      body
  in
  match final_result with
  | FinalForm (c, f) ->
    CompileError (Srcloc.start, "unimplemented create output")
  | UpdateCompiler c ->
    CompileError (Srcloc.start, "mod form must end on an expression")
  | FormError (l, e) -> CompileError (l, e)

let compile_file opts content =
  let parse_result =
    parse_sexp
      Srcloc.combineSrcLocation
      Srcloc.start
      Srcloc.advance
      content
  in
  let compiler = empty in
  match parse_result with
  | Sexp.Failure (loc, err) -> CompileError (loc, err)
  | Sexp.Success pre_forms ->
    let forms = strip_useless pre_forms in
    begin
      match forms with
      | [] ->
        CompileError (Srcloc.start, "Empty source file")
      | [Cons (_, Atom (_, "mod"), modbody)] ->
        begin
          match modbody with
          | Cons (_, args, Nil _) ->
            CompileError (Srcloc.start, "No forms in mod")
          | Cons (_, args, modforms) ->
            compile_mod compiler opts args @@ body_cons_to_list modforms
            |> compMap Sexp.to_string
          | _ ->
            CompileError (Srcloc.start, "Malformed mod form")
        end
      | (Cons (_, Atom (modloc, "mod"), modbody)) :: _ ->
        CompileError (modloc, "Only one mod allowed")
      | body ->
        (* A list of forms is like (mod () ...) *)
        compile_mod compiler opts (Nil Srcloc.start) body
        |> compMap Sexp.to_string
    end
