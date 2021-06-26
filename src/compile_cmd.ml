type arg_process =
  { includeDirs : string list
  ; nextInclude : bool
  ; inputFiles : string list
  }

let process_arg ap arg =
  if ap.nextInclude then
    { ap with nextInclude = false ; includeDirs = arg :: ap.includeDirs }
  else if String.length arg == 2 && arg == "-I" then
    { ap with nextInclude = true }
  else if String.length arg > 2 && String.sub arg 0 2 == "-I" then
    let newInclude = String.sub arg 2 ((String.length arg) - 2) in
    (* Prepend is fine: later dirs override earlier *)
    { ap with includeDirs = newInclude :: ap.includeDirs }
  else
    (* Will be reversed *)
    { ap with inputFiles = arg :: ap.inputFiles }

let main args =
  let ap =
    { includeDirs = []
    ; nextInclude = false
    ; inputFiles = []
    }
  in
  let ap = List.fold_left process_arg ap args in
  List.iter
    (fun infile ->
       let input =
         Node.Fs.readFileSync infile `utf8
       in
       let opts : Compiler.compilerOpts =
         { includeDirs = ap.includeDirs
         ; filename = infile
         ; readNewFile = fun _ _ _ ->
             Compiler.CompileError (Srcloc.start, "include unimplemented")
         }
       in
       let result = Compiler.compile_file opts input in
       match result with
       | CompileOk output ->
         Node.Fs.writeFileSync infile output `utf8
       | CompileError (srcloc, err) ->
         Printf.printf "%s(%s): %s\n" infile (Srcloc.toString srcloc) err
    )
    ap.inputFiles

let _ =
  let argv = Node.Process.argv in
  let real_args =
    Array.to_list @@ Array.sub argv 2 ((Array.length argv) - 2)
  in
  let want_help =
    real_args = [] ||
    (List.filter ((=) "-h") real_args) <> [] ||
    (List.filter ((=) "--help") real_args) <> []
  in
  if want_help then
    Printf.printf "Usage: chiacompile [-Iinclude_dir ...] [files]\n"
  else
    main real_args
