open Sexp

let usage () =
  Printf.printf "Usage: clvmrun [file] '[arg-atom-or-list]'\n"

let main args =
  match args with
  | [infile;args] ->
    begin
      let incode = Node.Fs.readFileSync infile `utf8 in
      match Clvm.parse_and_run infile incode args with
      | Clvm.RunOk v -> Printf.printf "%s" (to_string v)
      | Clvm.RunExn (l,e) ->
        Printf.printf "%s(%s): throw(x) %s\n" infile (Srcloc.toString l) (to_string e)
      | Clvm.RunError (l,e) ->
        Printf.printf "%s(%s): %s\n" infile (Srcloc.toString l) e
    end
  | _ -> usage ()

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
    usage ()
  else
    main real_args
