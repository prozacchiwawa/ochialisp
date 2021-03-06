open Comptypes
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
  ; { expected = RunOk "(30000 . 3392)"
    ; input = "(divmod (1 . 300000003392) (1 . 10000000))"
    ; args = "()"
    }
  ]

let compile_tests : RunCompileTest.t list =
  [ { expected = CompileOk "(2 (1 16 (1 . 1) (1 . 3)) (4 (1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defmacro testmacro (A) (qq (+ 1 (unquote A)))) (testmacro 3))"
    }

  ; { expected = CompileOk "(2 (1 2 (3 (1) (1 2 (1 16 (1 . 1) (1 . 3)) 1) (1 2 (1 16 (1 . 5) (1 . 8)) 1)) 1) (4 (1) 1))"
    ; opts = { emptyOpts with assemble = false ; stdenv = false }
    ; input = "(mod () (defmacro if (A B C) (qq (a (i (unquote A) (com (unquote B)) (com (unquote C))) @))) (if () (+ 1 3) (+ 5 8)))"
    }

  ; { expected = CompileOk "(2 (1 2 (3 (1) (1 2 (1 16 (1 . 1) (1 . 3)) 1) (1 2 (1 16 (1 . 5) (1 . 8)) 1)) 1) (4 (1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (if () (+ 1 3) (+ 5 8)))"
    }

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 (1 . 3) ()))) (4 (1 2 (1 16 5 (1 . 1)) 1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun f (a) (+ a 1)) (f 3))"
    }

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 (1 . 3) (4 (1 . 1) ())))) (4 (1 2 (1 16 (18 5 5) 11) 1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun f (a b) (+ (* a a) b)) (f 3 1))"
    }

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 5 ()))) (4 (1 2 (1 2 (3 (9 5 (1 . 1)) (1 2 (1 1 . 1) 1) (1 2 (1 18 (2 2 (4 2 (4 (17 5 (1 . 1)) ()))) 5) 1)) 1) 1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (arg_one) (defun factorial (input) (if (= input 1) 1 (* (factorial (- input 1)) input))) (factorial arg_one))"
    }

  ; { expected = CompileOk "(2 (1 2 2 (4 2 (4 (1 1 2 3) ()))) (4 (1 2 (1 2 (3 5 (1 2 (1 4 (1 . 4) (4 (5 5) (4 (2 2 (4 2 (4 (6 5) ()))) (1)))) 1) (1 2 (1 1) 1)) 1) 1) 1))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun makelist (a) (if a (c (q . 4) (c (f a) (c (makelist (r a)) (q . ())))) (q . ()))) (makelist (q . (1 2 3))))"
    }
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

  ; { expected = RunOk "120"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (arg_one) (defun factorial (input) (if (= input 1) 1 (* (factorial (- input 1)) input))) (factorial arg_one))"
    ; args = "(5)"
    }

  ; { expected = RunOk "(4 1 (4 2 (4 3 ())))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod () (defun makelist (a) (if a (c (q . 4) (c (f a) (c (makelist (r a)) (q . ())))) (q . ()))) (makelist (q . (1 2 3))))"
    ; args = "()"
    }

  ; { expected = RunOk "(1 2)"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (a) (list 1 2))"
    ; args = "()"
    }

  ; { expected = RunOk "(100 81 64 49)"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod args (defmacro square (input) (qq (* (unquote input) (unquote input)))) (defun sqre_list (my_list) (if my_list (c (square (f my_list)) (sqre_list (r my_list))) my_list)) (sqre_list args))"
    ; args = "(10 9 8 7)"
    }

  ; { expected = RunOk "((51 305419896 1000000000))"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (PASSWORD_HASH password new_puzhash amount) (defconstant CREATE_COIN 51) (defun check_password (PASSWORD_HASH password new_puzhash amount) (if (= (sha256 password) PASSWORD_HASH) (list (list CREATE_COIN new_puzhash amount)) (x))) (check_password PASSWORD_HASH password new_puzhash amount))"
    ; args = "(0x2ac6aecf15ac3042db34af4863da46111da7e1bf238fc13da1094f7edc8972a1 \"sha256ftw\" 0x12345678 1000000000)"
    }

  ; { expected = RunOk "15"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (a b) (let ((x (+ a 1)) (y (+ b 1))) (+ x y)))"
    ; args = "(5 8)"
    }

  ; { expected = RunOk "1"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (a) (defun f (i) (let ((x (not i)) (y (* i 2))) (+ x y))) (f a))"
    ; args = "(0)"
    }

  ; { expected = RunOk "6"
    ; opts = { emptyOpts with assemble = false }
    ; input = "(mod (a) (defun f (i) (let ((x (not i)) (y (* i 2))) (+ x y))) (f a))"
    ; args = "(3)"
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
