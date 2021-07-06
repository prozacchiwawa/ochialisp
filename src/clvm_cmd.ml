open Sexp

let usage () =
  Printf.printf "Usage: clvmrun [file] '[arg-atom-or-list]'\n"

let main args =
  match args with
  | [infile;args] ->
    begin
      let incode = Node.Fs.readFileSync infile `utf8 in
      Printf.printf "%s" @@
      Runtypes.run_to_string to_string @@
      Clvm.parse_and_run infile incode args
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
