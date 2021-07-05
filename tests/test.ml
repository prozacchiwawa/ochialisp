open Comptypes
open Clvm
open Compiler
open Frontend
open Codegen
open Runtypes

type test_spec =
  { expected : string compileResult
  ; opts : compilerOpts
  ; input : string
  }

type clvm_spec =
  { expected : string runResult
  ; input : string
  ; args : string
  }

let emptyOpts =
  { includeDirs = []
  ; filename = "test.clvm"
  ; assemble = true
  ; stdenv = true
  ; compiler = None

  ; readNewFile =
      (fun _ _ name ->
         if name == "*macros*" then
           CompileOk (name, String.concat "\n" Macros.macros)
         else
           CompileError (Srcloc.start name, "include unimplemented for name " ^ name)
      )

  ; compileProgram =
      (fun opts program ->
         frontend opts [program]
         |> compBind (fun m -> codegen opts m)
      )
  }

let emptyCompile = { emptyOpts with assemble = true }

let tests =
  [ { expected = CompileOk "(16 (1 . 1) (1 . 3))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defmacro testmacro (A) (qq (+ 1 (unquote A)))) (testmacro 3))"
    }

  ; { expected = CompileOk "(2 (3 (1) (1 16 (1 . 1) (1 . 3)) (1 16 (1 . 5) (1 . 8))) 1)"
    ; opts = { emptyOpts with assemble = false ; stdenv = false }
    ; input = "(mod () (defmacro if (A B C) (qq (a (i (unquote A) (com (unquote B)) (com (unquote C))) @))) (if () (+ 1 3) (+ 5 8)))"
    }

  ; { expected = CompileOk "(2 (3 (1) (1 16 (1 . 1) (1 . 3)) (1 16 (1 . 5) (1 . 8))) 1)"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (if () (+ 1 3) (+ 5 8)))"
    }
  ]

let clvm_tests =
  [ { expected = RunOk "(\"there\" \"fool\")"
    ; input = "(a (q 2 4 (c 2 (c 6 ()))) (c (q 13 26729 \"there\" \"fool\") 1))"
    ; args = "()"
    }
  ; { expected = RunOk "(4 1 (4 2 ()))"
    ; input = "(a (q 2 (q 2 2 (c 2 (c 3 (q)))) (c (q 2 (i 5 (q 4 (q . 4) (c 9 (c (a 2 (c 2 (c 13 (q)))) (q)))) (q 1)) 1) 1)) 1)"
    ; args = "(1 2)"
    }
  ]

let _ =
  let failed = ref false in
  let _ =
    clvm_tests
    |> List.iteri
      (fun i e ->
         let cr = parse_and_run "*clvm*" e.input e.args |> runMap Sexp.to_string in
         if cr <> e.expected then
           begin
             failed := true ;
             Js.log "\nunexpected output:" ;
             Js.log cr ;
             Js.log "for input:" ;
             Js.log @@ e.input ^ " on " ^ e.args ;
             Js.log "wanted:" ;
             Js.log e.expected
           end
         else
           Printf.printf "\r%s\r" (string_of_int i)
      )
  in
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
             Js.log "wanted:" ;
             Js.log e.expected
           end
         else
           Printf.printf "\r%s\r" (string_of_int i)
      )
  in
  (* Known bug in node: output doesn't always drain when destroying the process *)
  Js.Global.setTimeout
    (fun _ ->
       if !failed then
         begin
           Js.log "\nTests FAILED\n" ;
           Node.Process.process##abort ()
         end
       else
         Js.log "\nTests PASSED\n"
    )
    500
