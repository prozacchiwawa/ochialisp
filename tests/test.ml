open Comptypes
open Clvm
open Compiler
open Frontend
open Codegen
open Runtypes
open Testspec

let clvm_tests : RunExecTest.t list =
  [ { expected = RunOk "(\"there\" \"fool\")"
    ; input = "(a (q 2 4 (c 2 (c 6 ()))) (c (q 13 26729 \"there\" \"fool\") 1))"
    ; args = "()"
    }
  ; { expected = RunOk "(4 1 (4 2 ()))"
    ; input = "(a (q 2 (q 2 2 (c 2 (c 3 (q)))) (c (q 2 (i 5 (q 4 (q . 4) (c 9 (c (a 2 (c 2 (c 13 (q)))) (q)))) (q 1)) 1) 1)) 1)"
    ; args = "(1 2)"
    }
  ; { expected = RunOk "13"
    ; input = "(2 (3 (1) (1 16 (1 . 1) (1 . 3)) (1 16 (1 . 5) (1 . 8))) 1)"
    ; args = "()"
    }
  ]

let compile_tests : RunCompileTest.t list =
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

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 (1 . 3) ()))) (4 (1 16 5 (1 . 1)) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun f (a) (+ a 1)) (f 3))"
    }

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 (1 . 3) (4 (1 . 1) ())))) (4 (1 16 (18 5 5) 11) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun f (a b) (+ (* a a) b)) (f 3 1))"
    }

(*
  ; { expected = CompileOk "()"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun makelist (a) (if a (c (q . 4) (f a) (makelist (r a))) (q . ()))) (makelist (q . (1 2 3))))"
    }
   *)
  ]

let full_tests : RunFullTest.t list =
  [ { expected = RunOk "10"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun f (a b) (+ (* a a) b)) (f 3 1))"
    ; args = "()"
    }
  ; { expected = RunOk "13"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (c) (defun f (a b) (+ (* a a) b)) (f 3 c))"
    ; args = "(4)"
    }
  ]

let _ =
  let failed = ref false in
  let _ =
    clvm_tests |> List.iteri (RunExecTest.run failed)
  in
  let _ =
    compile_tests |> List.iteri (RunCompileTest.run failed)
  in
  let _ =
    full_tests |> List.iteri (RunFullTest.run failed)
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
