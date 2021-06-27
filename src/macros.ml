open Sexp

let macros =
  [ "(defmacro list ARGS ((c (mod args (defun compile-list (args) (if args (qq (c (unquote (f args)) (unquote (compile-list (r args))))) ())) (compile-list args)) ARGS)))"
  ]
