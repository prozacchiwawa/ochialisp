// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


var macros = {
  hd: "(defmacro if (A B C) (qq (a (i (unquote A) (com (unquote B)) (com (unquote C))) @)))",
  tl: {
    hd: "(defmacro list args (defun makelist (args) (if args (c (q . c) (c (f args) (c (makelist (r args)) (q . ())))) (q . ()))) (makelist args))",
    tl: /* [] */0
  }
};

exports.macros = macros;
/* No side effect */