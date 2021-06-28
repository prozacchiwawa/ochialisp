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
  | Quote

type compiler =
  { parent : compiler option
  ; used : StringSet.t
  ; symbols : symbolLookupResult StringMap.t
  }

let empty =
  let symbols =
    List.fold_left
      (fun coll (n,v) ->
         StringMap.add n (Prim v) coll
      )
      StringMap.empty
      Prims.prims
    |> StringMap.add "quote" Quote
  in
  { parent =
      Some
        { parent = None
        ; used = StringSet.empty
        ; symbols = symbols
        }
  ; used = StringSet.empty
  ; symbols = StringMap.empty
  }

let push_frame compiler =
  { parent = Some compiler
  ; used = StringSet.empty
  ; symbols = StringMap.empty
  }

type 'c compileFormResult =
  | FinalForm of 'c * Srcloc.t sexp
  | FormError of string * Srcloc.t * string
  | UpdateCompiler of 'c

let string_of_form_result = function
  | FinalForm (_, s) -> Printf.sprintf "(FinalForm %s)" (to_string s)
  | FormError (f, l, e) -> Printf.sprintf "(FormError %s(%s): %s" f (Srcloc.toString l) e
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

let rec lookup_symbol compiler name =
  try
    Some (StringMap.find name compiler.symbols)
  with _ ->
    begin
      match compiler.parent with
      | None -> None
      | Some c -> lookup_symbol c name
    end

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
    let llen = cons_length (Cons (l,a,b)) in
    let resarray = Array.make llen (Nil l) in
    let ccons = ref @@ Cons (l,a,b) in
    let idx = ref 0 in
    let _ =
      while not @@ nilp !ccons do
        let _ = Array.set resarray !idx @@ fst !ccons in
        let _ = idx := !idx + 1 in
        ccons := snd !ccons
      done
    in
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

let compile_defun compiler opts l args body : compiler compileFormResult =
  FormError (opts.filename, l, "defun not implemented yet")

let compile_inline compiler opts l args body : compiler compileFormResult =
  FormError (opts.filename, l, "defun-inline not implemented yet")

let compile_expand_macro compiler opts l s args : compiler compileFormResult =
  let run_result = Clvm.run s args in
  match run_result with
  | RunOk r ->
    let _ = Js.log @@ "run macro " ^ to_string s ^ " with " ^ to_string args in
    let _ = Js.log @@ "get output " ^ to_string r in
    FinalForm (compiler, r)
  | RunError (l,e) -> FormError (opts.filename,l,e)

let rec compile_call_function compiler opts l func args : compiler compileFormResult =
  let rec evaluated_args arg =
    match arg with
    | Cons (l,a,b) ->
      eval compiler opts l a
      |> compBind
        (fun n ->
           evaluated_args b
           |> compMap (fun atl -> Cons (l, n, atl))
        )
    | any -> eval compiler opts l any
  in
  let eval_function compiler arglist : compiler compileFormResult =
    match func with
    | Macro s ->
      compile_expand_macro compiler opts l s arglist
    | FunDef body ->
      FormError (opts.filename, l, "cannot call defun yet")
    | Constant _ ->
      FormError (opts.filename, l, "cannot invoke constant as a toplevel form")
    | FunctionArg (loc,id) ->
      FinalForm (compiler, Cons (l, Atom (loc, "a"), Integer (loc, string_of_int id)))
    | Prim p -> FinalForm (compiler, Cons (l,p,arglist))
  in
  match evaluated_args args with
  | CompileError (f,l,e) -> FormError (f,l,e)
  | CompileOk args -> eval_function compiler args

and eval compiler opts l arg =
  match arg with
  | Nil l -> CompileOk (Nil l)
  | Integer (l,i) -> CompileOk (Cons (l,Atom (l,"q"),Integer (l,i)))
  | QuotedString (l,q,i) -> CompileOk (QuotedString (l,q,i))
  | Atom (l,a) ->
    begin
      match lookup_symbol compiler a with
      | None -> CompileError (opts.filename,l,"unknown name " ^ a)
      | Some (FunctionArg (l,i)) -> CompileOk (Integer (l,string_of_int i))
      | Some (Constant v) -> CompileOk v
      | Some (Macro _) ->
        CompileError (opts.filename,l,"can't use macro " ^ a ^ " as a function argument")
      | Some (FunDef f) -> CompileOk f
      | Some (Prim p) -> CompileOk p
    end
  | Cons (l,Atom (_,head),rest) ->
    begin
      match compile_top_eval compiler opts l head rest with
      | FinalForm (_,f) -> CompileOk f
      | FormError (f,l,e) -> CompileError (f,l,e)
      | UpdateCompiler _ -> CompileError (opts.filename,l,"Wrong kind of form preceeding invocation")
    end
  | Cons (l,h,r) ->
    CompileError
      (opts.filename,l,"don't know how to eval " ^ to_string (Cons (l,h,r)))
  | Comment (l,_) ->
    CompileError (opts.filename,l,"comment got through to compile stage")
  | EmptyLine l ->
    CompileError (opts.filename,l,"emptyline made it to compile stage")

and compile_top_eval compiler opts l name args : compiler compileFormResult =
  let to_invoke = lookup_symbol compiler name in
  match to_invoke with
  | None ->
    FormError (opts.filename, l, "toplevel invoke of undefined symbol " ^ name)
  | Some Quote ->
    begin
      match args with
      | Cons (l,quoted,Nil _) ->
        FinalForm (compiler, Cons (l, Integer (l, "1"), quoted))
      | any ->
        FormError (opts.filename, l, "quote applied to malformed tail " ^ to_string any)
    end
  | Some (Prim (Integer (_, "1"))) ->
    (* Evaluating a "prim 1" does primitive style quoting. *)
    FinalForm (compiler, Cons (l, Integer (l, "1"), args))
  | Some other ->
    compile_call_function compiler opts l other args

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

and compile_macro compiler opts l name args body : compiler compileFormResult =
  let _ = Js.log @@ "macro body " ^ to_string body in
  let full_body = body_cons_to_list body in
  let _ = Js.log @@ "macro body list " ^ (String.concat ";" (List.map to_string full_body)) in
  compile_mod compiler opts args full_body
  |> compMap
    (fun f ->
       let _ = Js.log @@ "macro compiled: " ^ to_string f in
       let to_define_in =
         match lookup_symbol compiler name with
         | Some _ -> push_frame compiler
         | _ -> compiler
       in
       { to_define_in with
         symbols = StringMap.add name (Macro f) to_define_in.symbols
       }
    )
  |> compiler_result_to_update

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
  | Cons (l, Atom (_,"include"), Cons (_, QuotedString (_, _, incname), _)) ->
    include_file compiler opts incname
  | Cons (l, Atom (_,"defun"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_defun compiler opts l args body
  | Cons (l, Atom (_,"defmacro"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    let _ =
      Js.log @@
      "defmacro " ^ name ^ " args " ^ to_string args ^ " body " ^ to_string body
    in
    compile_macro compiler opts l name args body
  | Cons (l, Atom (_,"defun-inline"), Cons (_, Atom (_, name), Cons (_, args, body))) ->
    compile_inline compiler opts l args body
  | Cons (l, Atom (_,name), args) ->
    compile_top_eval compiler opts l name args
  | Cons (l, x, y) ->
    FormError (opts.filename, l, "malformed toplevel form " ^ (to_string (Cons (l,x,y))))
  | Nil l -> FinalForm ({ compiler with used = StringSet.empty }, Nil l)
  | Comment (_,_) -> UpdateCompiler compiler
  | EmptyLine _ -> UpdateCompiler compiler

and compile_mod_form compiler opts form =
  let _ = Js.log "compile_mod_form" in
  let _ = Js.log @@ to_string form in
  compile_mod_form_ compiler opts form

and make_arg_list opts d n (a,b) : (Srcloc.t * string * int) list compileResult =
  let addend_atom side = if side then d / 2 else 0 in
  let addend_cons side = if side then d else 0 in
  let make_side (side : bool) = function
    | Atom (l,name) -> CompileOk [(l, name, d + n + addend_atom side)]
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

and compile_mod (compiler : compiler) opts args body =
  let mod_args =
    match args with
    | Nil _ -> CompileOk []
    | Cons (_,a,b) -> make_arg_list opts 2 0 (a,b)
    | Atom (l,v) -> CompileOk [(l, v, 1)]
    | QuotedString (l,_,_) ->
      CompileError (opts.filename,l,"quoted string given as argument list for mod")
    | Integer (l,_) ->
      CompileError (opts.filename,l,"integer given as argument list for mod")
    | x -> CompileError (opts.filename,Srcloc.start, "unexpected token as mod argument list")
  in
  let compiler_with_args =
    mod_args
    |> compMap
      (fun args ->
         { compiler with
           symbols =
             List.fold_left
               (fun coll (l,k,v) ->
                  StringMap.add k (FunctionArg (l,v)) coll
               )
               compiler.symbols
               args
         }
      )
  in
  let process_mod_form
      (compiler : compiler compileFormResult)
      (form : Srcloc.t sexp) :
    compiler compileFormResult =
    compiler
    |> formBind opts.filename (fun (c : compiler) -> compile_mod_form c opts form)
  in
  compiler_with_args
  |> compBind
    (fun compiler ->
       let final_result = List.fold_left process_mod_form (UpdateCompiler compiler) body in
       let _ = Js.log "processed mod" in
       let _ = Js.log @@ string_of_form_result final_result in
       match final_result with
       | FinalForm (c, f) ->
         CompileOk f
       | UpdateCompiler c ->
         CompileError (opts.filename, Srcloc.start, "mod form must end on an expression")
       | FormError (f, l, e) -> CompileError (f, l, e)
    )

and include_macros lst =
  let include_stmt =
    Cons
      ( Srcloc.start
      , Atom (Srcloc.start, "include")
      , Cons (Srcloc.start, QuotedString (Srcloc.start, '\"', "*macros*"), Nil Srcloc.start)
      )
  in
  include_stmt :: lst

and compile_to_assembler opts pre_forms : Srcloc.t sexp compileResult =
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
          let body_list = include_macros @@ body_cons_to_list modforms in
          let _ = Js.log @@ String.concat ";" (List.map to_string body_list) in
          compile_mod compiler opts args body_list
        | _ ->
          CompileError (opts.filename, Srcloc.start, "Malformed mod form")
      end
    | (Cons (_, Atom (modloc, "mod"), modbody)) :: _ ->
      CompileError (opts.filename, modloc, "Only one mod allowed")
    | body ->
      (* A list of forms is like (mod () ...) *)
      compile_mod compiler opts (Nil Srcloc.start) @@ include_macros body
  end

and compile_file opts content : string compileResult =
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
