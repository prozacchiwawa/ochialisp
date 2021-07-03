open Sexp

let primloc = Srcloc.start "*prims*"

let prims : (string * Srcloc.t sexp) list =
  [ ("q", Integer (primloc, "1"))
  ; ("a", Integer (primloc, "2"))
  ; ("i", Integer (primloc, "3"))
  ; ("c", Integer (primloc, "4"))
  ; ("f", Integer (primloc, "5"))
  ; ("r", Integer (primloc, "6"))
  ; ("l", Integer (primloc, "7"))
  ; ("x", Integer (primloc, "8"))
  ; ("=", Integer (primloc, "9"))
  ; (">s", Integer (primloc, "10"))
  ; ("sha256", Integer (primloc, "11"))
  ; ("substr", Integer (primloc, "12"))
  ; ("strlen", Integer (primloc, "13"))
  ; ("concat", Integer (primloc, "14"))
  ; ("+", Integer (primloc, "16"))
  ; ("-", Integer (primloc, "17"))
  ; ("*", Integer (primloc, "18"))
  ; ("/", Integer (primloc, "19"))
  ; ("divmod", Integer (primloc, "20"))
  ; (">", Integer (primloc, "21"))
  ; ("ash", Integer (primloc, "22"))
  ; ("lsh", Integer (primloc, "23"))
  ; ("logand", Integer (primloc, "24"))
  ; ("logior", Integer (primloc, "25"))
  ; ("logxor", Integer (primloc, "26"))
  ; ("lognot", Integer (primloc, "27"))
  ; ("point_add", Integer (primloc, "29"))
  ; ("pubkey_for_exp", Integer (primloc, "30"))
  ; ("not", Integer (primloc, "32"))
  ; ("any", Integer (primloc, "33"))
  ; ("all", Integer (primloc, "34"))
  ; ("softfork", Integer (primloc, "36"))
  ]

let primquote l a = Cons (l, Integer (l,"1"), a)

let primcons l a b = Cons (l, Integer (l,"4"), Cons (l, a, Cons (l, b, Nil l)))

let primapply l a b = Cons (l, Integer (l,"2"), Cons (l, a, Cons (l, b, Nil l)))

let primop l op args = Cons (l, op, args)

