open Sexp

let macros =
  [ "(defmacro if (A B C) (qq (a (i (unquote A) (function (unquote B)) (function (unquote C))) @)))"
  ; "(defmacro function (BODY) (qq (opt (com (q . (unquote BODY)) (qq (unquote (macros))) (qq (unquote (symbols)))))))"
  ; "(defmacro list args (defun compile-list (args) (if args (qq (c (unquote (f args)) (unquote (compile-list (r args))))) (compile-list args))))"
  ]
