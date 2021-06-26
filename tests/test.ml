open Compiler

type test_spec =
  { expected : string compileResult
  ; opts : compilerOpts
  ; input : string
  }

let emptyOpts =
  { includeDirs = []
  ; filename = "test.clvm"
  ; assemble = true
  ; readNewFile = fun _ name _ ->
      Compiler.CompileError (name, Srcloc.start, "include unimplemented")
  }

let emptyCompile = { emptyOpts with assemble = true }

let tests =
  [ { expected = CompileOk "00"
    ; opts = emptyOpts
    ; input = "()"
    }
  ]

let _ =
  let failed = ref false in
  let _ =
    tests
    |> List.iteri
      (fun i e ->
         let cr = compile_file e.opts e.input in
         if cr <> e.expected then
           begin
             failed := true ;
             Js.log "\nunexpected output:" ;
             Js.log cr ;
             Js.log "for input:" ;
             Js.log e.input ;
             Node.Process.process##abort ()
           end
         else
           Js.log ("\r" ^ (string_of_int i) ^ "\r")
      )
  in
  Js.log "\nTests PASSED\n"
