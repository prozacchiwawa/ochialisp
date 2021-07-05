let macros =
  [ "(defmacro if (A B C) (qq (a (i (unquote A) (com (unquote B)) (com (unquote C))) @)))"
  ; "(defmacro list args (defun compile-list (args) (if args (qq (c (unquote (f args)) (unquote (compile-list (r args))))) (compile-list args))))"
  ]
