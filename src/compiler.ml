open Sexp

type 'a compileResult =
  | CompileOk of 'a
  | CompileError of string * Srcloc.t * string

type compilerOpts =
  { includeDirs : string list
  ; filename : string
  ; readNewFile : compilerOpts -> string -> string -> (string * string) compileResult
  ; assemble : bool
  }

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbolLookupResult
  = FunctionArg of Srcloc.t * int
  | Constant of Srcloc.t sexp
  | Macro of Srcloc.t sexp
  | FunDef of Srcloc.t sexp
  | Prim of Srcloc.t sexp

type compiler =
  { parent : compiler option
  ; used : StringSet.t
  ; symbols : symbolLookupResult StringMap.t
  }

let empty =
  { parent =
      Some
        { parent = None
        ; used = StringSet.empty
        ; symbols =
            List.fold_left
              (fun coll (n,v) ->
                 StringMap.add n (Prim v) coll
              )
              StringMap.empty
              Prims.prims
        }
  ; used = StringSet.empty
  ; symbols = StringMap.empty
  }

type 'c compileFormResult =
  | FinalForm of 'c * Srcloc.t sexp
  | FormError of string * Srcloc.t * string
  | UpdateCompiler of 'c

let string_of_form_result = function
  | FinalForm (_, s) -> "(FinalForm " ^ to_string s ^ ")"
  | FormError (_, _, e) -> "(FormError " ^ e ^ ")"
  | UpdateCompiler _ -> "UpdateCompiler"

let cons_length c =
  let counted = ref 0 in
  let current = ref @@ c in
  let _ =
    while listp !current && not (nilp !current) do
      let _ = counted := !counted + 1 in
      current := snd !current
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
  | CompileError (f,l,e) -> FormError (f,l,e)

let compiler_result_to_form = function
  | CompileOk (u,r) -> FinalForm (u,r)
  | CompileError (f,l,e) -> FormError (f,l,e)

(* reason on js doesn't do tail recursion, so do this with an array
 * as an intermediate.
 *)
let body_cons_to_list = function
  | Nil _ -> []
  | Cons (l,a,b) ->
    let _ = Js.log "body_cons_to_list" in
    let _ = Js.log @@ to_string (Cons (l,a,b)) in
    let llen = cons_length (Cons (l,a,b)) in
    let _ = Js.log llen in
    let resarray = Array.make llen (Nil l) in
    let ccons = ref @@ Cons (l,a,b) in
    let idx = ref 0 in
    let _ =
      while not @@ nilp @@ snd !ccons do
        let _ = Array.set resarray !idx @@ fst !ccons in
        let _ = idx := !idx + 1 in
        ccons := snd !ccons
      done
    in
    let _ = Js.log "body_cons_to_list" in
    let _ = Js.log resarray in
    Array.to_list resarray
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
  | CompileError (f,l,e) -> CompileError (f,l,e)

let compBind f = function
  | CompileOk r -> f r
  | CompileError (f,l,e) -> CompileError (f,l,e)

let formBind srcfile f = function
  | UpdateCompiler c -> f c
  | FormError (f,l,e) -> FormError (f,l,e)
  | FinalForm (_,f) -> FormError (srcfile, location_of f, "A nonbinding final form was already processed")

let formMap (f : 'c -> 'd) = function
  | UpdateCompiler c -> UpdateCompiler (f c)
  | any -> any

let rec lower_quote_element opts = function
  | Nil l -> CompileOk (Nil l)
  | Cons (l,a,Cons (x,b,c)) ->
    lower_quote_element opts (Cons (x,b,c))
    |> compBind
      (fun rest ->
         lower_quote opts a
         |> compMap (fun first -> Cons (l,first,rest))
      )
  | Cons (l,a,b) ->
    lower_quote opts b
    |> compBind
      (fun rest ->
         lower_quote opts a
         |> compMap (fun first -> Cons (l,first,rest))
      )
  | a -> lower_quote opts a

and lower_quote opts prog =
  match prog with
  | Nil l -> CompileOk (Nil l)
  | Cons (l,Atom (_, "quote"),Cons (_,obj,Nil _)) ->
    (* Correct quote *)
    lower_quote opts obj
  | Cons (_,Atom (l, "quote"),_) ->
    CompileError (opts.filename,l,"Malformed quote (too many tails)")
  | Cons (l,a,b) -> lower_quote_element opts (Cons (l,a,b))
  | a -> CompileOk a

let compile_defun compiler opts l args body : compiler compileFormResult =
  FormError (opts.filename, l, "defun not implemented yet")

let compile_macro compiler opts l args body : compiler compileFormResult =
  FormError (opts.filename, l, "defmacro not implemented yet")

let compile_inline compiler opts l args body : compiler compileFormResult =
  FormError (opts.filename, l, "defun-inline not implemented yet")

let compile_expand_macro compiler opts l s args : compiler compileFormResult =
  FormError (opts.filename, l, "can't expand macro yet")

let compile_call_function compiler opts l body args : (compiler * Srcloc.t sexp) compileResult =
  CompileError (opts.filename, l, "can't call function yet")

let eval compiler opts l arg =
  CompileError (opts.filename, l, "can't eval yet")

let compile_top_eval compiler opts l name args : compiler compileFormResult =
  let to_invoke = lookup_symbol compiler name in
  let evaluated_args =
    List.fold_left
      (fun alist arg ->
         alist
         |> compBind
           (fun atl ->
              eval compiler opts l arg |> compMap (fun n -> n :: atl)
           )
      )
      (CompileOk [])
      args
  in
  let eval_function compiler arglist : compiler compileFormResult =
    match to_invoke with
    | None -> FormError (opts.filename, l, "no symbol " ^ name ^ " at toplevel context")
    | Some (Macro s) ->
      compile_expand_macro compiler opts l s arglist
    | Some (FunDef body) ->
      compile_call_function compiler opts l body arglist
      |> compiler_result_to_form
    | Some (Constant _) ->
      FormError (opts.filename, l, "cannot invoke constant " ^ name ^ " as a toplevel form")
    | Some (FunctionArg (loc,id)) ->
      compile_call_function compiler opts l (Cons (l, Atom (loc, "a"), Integer (loc, string_of_int id))) (list_to_cons arglist)
      |> compiler_result_to_form
    | Some (Prim p) ->
      FinalForm (compiler, p)
  in
  match evaluated_args with
  | CompileError (f,l,e) -> FormError (f,l,e)
  | CompileOk args -> eval_function compiler args

let rec include_file compiler opts incfile : compiler compileFormResult =
  opts.readNewFile opts opts.filename incfile
  |> compiler_result_to_update
  |> formBind incfile
    (fun (new_file_name, new_file_content) ->
      let parse_result =
        parse_sexp
          Srcloc.combineSrcLocation
          Srcloc.start
          Srcloc.advance
          new_file_content
      in
      match parse_result with
      | Sexp.Failure (loc, err) -> FormError (opts.filename, loc, err)
      | Sexp.Success pre_forms ->
        List.fold_left
          (fun compiler form ->
             compiler
             |> formBind opts.filename
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

and compile_mod_form_ compiler opts : Srcloc.t sexp -> compiler compileFormResult =
  function
  | QuotedString (l,ch,q) ->
    FinalForm (compiler, Cons (l, Atom (l,"q"), QuotedString (l,ch,q)))
  | Integer (l, v) ->
    FinalForm (compiler, Cons (l, Atom (l,"q"), Atom (l,v)))
  | Atom (l, v) ->
    begin
      match lookup_symbol compiler v with
      | None -> FormError (opts.filename, l, "unknown name in context " ^ v)
      | Some (FunctionArg (_, n)) ->
        (* No need for arg rewriting since it's a toplevel form *)
        FinalForm (compiler, Atom (l, string_of_int n))
      | Some (Constant value) ->
        (* Constant as a toplevel form just quotes the value *)
        FinalForm (compiler, Cons (l, Atom (l, "q"), value))
      | Some (Macro _) ->
        (* Can't use a bare macro name as a toplevel form *)
        FormError (opts.filename, l, v ^ " names a macro and can't be used as an atom used as a toplevel form")
      | Some (FunDef f) ->
        (* In lisp, a function has no value as an atom *)
        FormError (opts.filename, l, v ^ " lisp doesn't define the value of a defun'd atom")
      | Some (Prim p) ->
        FinalForm (compiler, p)
    end
  | Cons (l, Atom (_,"include"), Cons (_, Atom (_, incname), _)) ->
    include_file compiler opts incname
  | Cons (l, Atom (_,"defun"), Cons (_, Atom (_, name), Cons (_, Atom (_, args), body))) ->
    compile_defun compiler opts l args body
  | Cons (l, Atom (_,"defmacro"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_macro compiler opts l args body
  | Cons (l, Atom (_,"defun-inline"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_inline compiler opts l args body
  | Cons (l, Atom (_,name), args) ->
    compile_top_eval compiler opts l name (body_cons_to_list args)
  | Cons (l, _, _) ->
    FormError (opts.filename, l, "malformed toplevel form")
  | Nil l -> FinalForm ({ compiler with used = StringSet.empty }, Nil l)
  | Comment (_,_) -> UpdateCompiler compiler
  | EmptyLine _ -> UpdateCompiler compiler

and compile_mod_form compiler opts form =
  let _ = Js.log "compile_mod_form" in
  let _ = Js.log @@ to_string form in
  compile_mod_form_ compiler opts form

let rec make_arg_list opts d n (a,b) : (string * int) list compileResult =
  let addend_atom side = if side then d / 2 else 0 in
  let addend_cons side = if side then d else 0 in
  let make_side (side : bool) = function
    | Atom (_,name) -> CompileOk [(name, d + n + addend_atom side)]
    | Cons (_,x,y) -> make_arg_list opts (2 * d) (n + addend_cons side) (x,y)
    | Nil l -> CompileError (opts.filename, l, "nil doesn't make sense in a param list")
    | Integer (l,v) -> CompileError (opts.filename, l, "int doesn't make sense in a param list")
    | QuotedString (l,_,_) -> CompileError (opts.filename, l, "quoted string doesn't make sense in a param list")
    | Comment (l,_) ->
      CompileError (opts.filename,l,"unexpected comment in arglist (should have been stripped")
    | EmptyLine l ->
      CompileError (opts.filename,l,"unexpected emptyline in arglist (should have been stripped")
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
    | Cons (_,a,b) -> make_arg_list opts 2 0 (a,b)
    | Atom (l,v) -> CompileOk [(v, 1)]
    | QuotedString (l,_,_) ->
      CompileError (opts.filename,l,"quoted string given as argument list for mod")
    | Integer (l,_) ->
      CompileError (opts.filename,l,"integer given as argument list for mod")
    | x -> CompileError (opts.filename,Srcloc.start, "unexpected token as mod argument list")
  in
  let initial_compiler = UpdateCompiler compiler in
  let process_mod_form
      (compiler : compiler compileFormResult)
      (form : Srcloc.t sexp) :
    compiler compileFormResult =
    compiler
    |> formBind opts.filename
      (fun (c : compiler) ->
         let about_to_compile = lower_quote opts form in
         match about_to_compile with
         | CompileOk f ->
           let r = compile_mod_form c opts f in
           let _ = Js.log "compile_mod_form -> " in
           let _ = Js.log @@ to_string f in
           let _ = Js.log "gives -> " in
           let _ = Js.log @@ string_of_form_result r in
           r
         | CompileError (f,l,e) -> FormError (f,l,e)
      )
  in
  let final_result = List.fold_left process_mod_form initial_compiler body in
  let _ = Js.log "processed mod" in
  let _ = Js.log @@ string_of_form_result final_result in
  match final_result with
  | FinalForm (c, f) ->
    CompileOk f
  | UpdateCompiler c ->
    CompileError (opts.filename, Srcloc.start, "mod form must end on an expression")
  | FormError (f, l, e) -> CompileError (f, l, e)

let compile_to_assembler opts pre_forms : Srcloc.t sexp compileResult =
  let compiler = empty in
  let forms = strip_useless pre_forms in
  begin
    match forms with
    | [] ->
      CompileError (opts.filename, Srcloc.start, "Empty source file")
    | [Cons (_, Atom (_, "mod"), modbody)] ->
      begin
        match modbody with
        | Cons (_, args, Nil _) ->
          CompileError (opts.filename, Srcloc.start, "No forms in mod")
        | Cons (_, args, modforms) ->
          let _ = Js.log @@ "modforms " ^ to_string modforms in
          let body_list = body_cons_to_list modforms in
          let _ = Js.log @@ String.concat ";" (List.map to_string body_list) in
          compile_mod compiler opts args body_list
        | _ ->
          CompileError (opts.filename, Srcloc.start, "Malformed mod form")
      end
    | (Cons (_, Atom (modloc, "mod"), modbody)) :: _ ->
      CompileError (opts.filename, modloc, "Only one mod allowed")
    | body ->
      (* A list of forms is like (mod () ...) *)
      compile_mod compiler opts (Nil Srcloc.start) body
  end

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
    |> compMap
      (fun result ->
         if opts.assemble then
           Sexp.encode result
         else
           Sexp.to_string result
      )
