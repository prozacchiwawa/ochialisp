let macros =
  [ "(defmacro if (A B C) (qq (a (i (unquote A) (com (unquote B)) (com (unquote C))) @)))"
  ; "(defmacro list args (defun makelist (args) (if args (c (q . c) (c (f args) (c (makelist (r args)) (q . ())))) (q . ()))) (makelist args))"
  ]
