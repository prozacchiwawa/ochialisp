type 'loc sexp
  = Nil of 'loc
  | Cons of ('loc * 'loc sexp * 'loc sexp)
  | Integer of ('loc * string)
  | QuotedString of ('loc * char * string)
  | Atom of ('loc * string)

let location_of = function
  | Nil l -> l
  | Cons (l,_,_) -> l
  | Integer (l,_) -> l
  | QuotedString (l,_,_) -> l
  | Atom (l,_) -> l

let make_cons ext a b = Cons (ext (location_of a) (location_of b), a, b)

type 'loc sexpParseState
  = Empty
  | CommentText of ('loc * string)
  | Bareword of ('loc * string)
  | QuotedText of ('loc * char * string)
  | QuotedEscaped of ('loc * char * string)
  | OpenList of 'loc
  | ParsingList of ('loc * 'loc sexpParseState * ('loc sexp -> 'loc sexp))
  | TermList of ('loc * 'loc sexpParseState * ('loc sexp -> 'loc sexp))

type 'loc sexpParseResult
  = PResume of ('loc sexpParseState)
  | PEmit of (('loc sexp) * ('loc sexpParseState))
  | PError of ('loc * string)

type integral = Decimal | Hex | NotIntegral

let matches_integral s =
  let is_hex () = String.length s >= 2 && String.sub s 0 2 == "0x" in
  let is_dec () =
    let first = ref true in
    let dec = ref true in
    let _ =
      for i = 0 to ((String.length s) - 1) do
        let _ =
          match String.get s i with
          | '0'..'9' -> ()
          | '-' -> if !first then () else dec := false
          | _ -> dec := false
        in
        first := false
      done
    in
    !dec && s <> "-"
  in
  if is_hex () then Hex else if is_dec () then Decimal else NotIntegral

let normalize_int v base =
  BigInteger.toString
    (BigInteger.bigIntBaseN (`String v) (`Int base))
    ~base:10 ()

let make_atom l v =
  let alen = String.length v in
  if alen > 1 && String.get v 0 == '#' then
    Atom (l,String.sub v 1 (alen - 1))
  else
    match matches_integral v with
    | Hex -> Integer (l,normalize_int (String.sub v 2 (alen - 2)) 16)
    | Decimal -> Integer (l,normalize_int v 10)
    | NotIntegral -> Atom (l,v)

let emit (a : 'loc sexp) (p : 'loc sexpParseState) : 'loc sexpParseResult =
  PEmit (a,p)

let error l t : 'loc sexpParseResult = PError (l, t)

let resume (p : 'loc sexpParseState) : 'loc sexpParseResult = PResume p

let rec sexp_to_json l2j = function
  | Nil _ -> Js.Json.null
  | Cons (l,h,t) ->
    [ ("location", l2j l)
    ; ("h", sexp_to_json l2j h)
    ; ("t", sexp_to_json l2j t)
    ]
    |> Js.Dict.fromList
    |> Js.Json.object_
  | Integer (l,v) ->
    [ ("integer", Js.Json.string v)
    ; ("location", l2j l)
    ]
    |> Js.Dict.fromList
    |> Js.Json.object_
  | QuotedString (l,t,s) ->
    [ ("string", Js.Json.string s)
    ; ("location", l2j l)
    ; ("quote", Js.Json.string (String.make 1 t))
    ]
    |> Js.Dict.fromList
    |> Js.Json.object_
  | Atom (l,a) ->
    [ ("atom", Js.Json.string a)
    ; ("location", l2j l)
    ]
    |> Js.Dict.fromList
    |> Js.Json.object_

let isspace = function
  | ' ' -> true
  | '\t' -> true
  | '\r' -> true
  | '\n' -> true
  | '\b' -> true
  | _ -> false

let rec parse_sexp_step (ext : 'loc -> 'loc -> 'loc) (loc : 'loc) : 'loc sexpParseState -> char -> 'loc sexpParseResult = function
  | Empty ->
    begin
      function
      | '(' -> resume @@ OpenList loc
      | '\n' -> resume @@ Empty
      | ';' -> resume @@ CommentText (loc, "")
      | ')' -> error loc "Too many close parens"
      | '"' -> resume @@ QuotedText (loc, '"', "")
      | '\'' -> resume @@ QuotedText (loc, '\'', "")
      | x ->
        if isspace x then
          resume Empty
        else
          resume @@ Bareword (loc, String.make 1 x)
    end
  | CommentText (pl, t) ->
    begin
      function
      | '\r' -> resume @@ CommentText (pl, t)
      | '\n' -> resume @@ Empty
      | x -> resume @@ CommentText (ext pl loc, t ^ (String.make 1 x))
    end
  | Bareword (pl, a) ->
    fun ch ->
      if isspace ch then
        emit (make_atom pl a) Empty
      else
        resume @@ Bareword (ext pl loc, a ^ (String.make 1 ch))
  | QuotedText (pl, term, t) ->
    begin
      function
      | '\\' -> resume @@ QuotedEscaped (pl, term, t)
      | ch ->
        if ch == term then
          emit (QuotedString (ext pl loc, term, t)) Empty
        else
          resume @@ QuotedText (pl, term, t ^ (String.make 1 ch))
    end
  | QuotedEscaped (pl, term, t) ->
    fun ch -> resume @@ QuotedText (pl, term, t ^ (String.make 1 ch))
  | OpenList pl ->
    begin
      function
      | ')' -> emit (Nil (ext pl loc)) Empty
      | '.' -> error loc "Dot can't appear directly after begin paren"
      | x ->
        begin
          match parse_sexp_step ext loc Empty x with
          | PEmit (o,p) -> resume @@ ParsingList (ext pl loc, p, make_cons ext o)
          | PResume p -> resume @@ ParsingList (ext pl loc, p, fun r -> r)
          | PError e -> PError e
        end
    end
  | ParsingList (pl, pp, list_content) ->
    begin
      fun ch ->
        match (ch, pp) with
        | ('.', Empty) -> resume @@ TermList (ext pl loc, Empty, list_content)
        | (')', Empty) ->
          emit (list_content (Nil loc)) Empty
        | (')', Bareword (l,t)) ->
          let parsed_atom = make_atom l t in
          let finished_list = make_cons ext parsed_atom (Nil loc) in
          emit (list_content finished_list) Empty
        | (ch, _) ->
          begin
            match parse_sexp_step ext loc pp ch with
            | PEmit (o,p) ->
              let
                result =
                ParsingList
                  ( ext pl loc
                  , p
                  , fun rest -> list_content (make_cons ext o rest)
                  )
              in
              resume result
            | PResume rp -> resume @@ ParsingList (ext pl loc, rp, list_content)
            | PError e -> PError e
          end
    end
  | TermList (pl, pp, list_content) ->
    begin
      fun ch ->
        match (ch, pp) with
        | ('.', Empty) -> error loc "Multiple dots in list notation are illegal"
        | (')', Empty) -> emit (list_content (Nil loc)) Empty
        | (')', Bareword (l,t)) ->
           let parsed_atom = make_atom l t in
           emit (list_content parsed_atom) Empty
        | (ch, _) ->
          begin
            match parse_sexp_step ext loc pp ch with
            | PEmit (o,p) ->
              let
                result =
                ParsingList
                  ( ext pl loc
                  , p
                  , fun _ -> list_content o
                  )
              in
              resume result
            | PResume p -> resume @@ TermList (ext pl loc, p, list_content)
            | PError e -> PError e
          end
    end

type 'loc finalParse
  = Success of ('loc sexp) list
  | Failure of ('loc * string)

let rec parse_sexp_inner ext start advance p n s =
  if n >= String.length s then
    match p with
    | Empty -> Success []
    | Bareword (l, t) -> Success [make_atom l t]
    | CommentText (_, _) -> Success []
    | QuotedText (l, _, _) -> Failure (l, "unterminated quoted string")
    | QuotedEscaped (l, _, _) -> Failure (l, "unterminated quoted string with escape")
    | OpenList l -> Failure (l, "Unterminated list (empty)")
    | ParsingList (l, _, _) -> Failure (l, "Unterminated mid list")
    | TermList (l, _, _) -> Failure (l, "Unterminated tail list")
  else
    let this_char = String.get s n in
    let next_location = advance start this_char in
    match parse_sexp_step ext start p this_char with
    | PError (l,e) -> Failure (l,e)
    | PResume np -> parse_sexp_inner ext next_location advance np (n+1) s
    | PEmit (o,np) ->
      match parse_sexp_inner ext next_location advance np (n+1) s with
      | Failure f -> Failure f
      | Success l -> Success (o :: l)

let parse_sexp (ext : 'loc -> 'loc -> 'loc) (start : 'loc) (advance : 'loc -> char -> 'loc) : string -> 'loc finalParse = parse_sexp_inner ext start advance Empty 0

let escape_quote q s =
  let basic_escape =
    List.init
      (String.length s)
      (fun i ->
         let ch = String.get s i in
         if ch == q then
           "\\" ^ (String.make 1 q)
         else
           String.make 1 ch
      )
  in
  String.concat "" basic_escape

let rec to_string = function
  | Nil _ -> "()"
  | Cons (_,a,b) -> "(" ^ (list_no_parens (a,b)) ^ ")"
  | Integer (_,v) -> v
  | QuotedString (_,q,s) -> "\"" ^ (escape_quote q s) ^ "\""
  | Atom (_,a) -> a

and list_no_parens = function
  | (a,Nil _) -> to_string a
  | (a,Cons (_,b,c)) ->
    to_string a ^ " " ^ list_no_parens (b,c)
  | (a,b) -> to_string a ^ " . " ^ to_string b

(* The zero integer contains only the characters 'x' and '0' *)
let is_zero_integer v =
  let other_symbol = ref false in
  let slen = String.length v in
  let _ =
    for i = 0 to (slen - 1) do
      match String.get v i with
      | '0' -> ()
      | 'x' -> ()
      | _ -> other_symbol := true
    done
  in
  not !other_symbol

let nilp = function
  | Nil _ -> true
  | QuotedString (_,_,"") -> true
  | Integer (_,v) -> is_zero_integer v
  | _ -> false

let listp = function
  | Nil _ -> true
  | Cons (_,_,_) -> true
  | _ -> false

let cons_fst = function
  | Cons (_,a,_) -> a
  | _ -> Nil Srcloc.start

let cons_snd = function
  | Cons (_,_,b) -> b
  | _ -> Nil Srcloc.start

let encode_hex_digit_list bi =
  let encoded = BigInteger.toString bi ~base:16 () in
  let enclen = String.length encoded in
  let padded = if enclen mod 2 == 0 then encoded else "0"^encoded in
  let elen = BigInteger.bigInt (`Int ((String.length padded) / 2)) in
  let len40 = BigInteger.bigInt (`Int 0x40) in
  let len2000 = BigInteger.bigInt (`Int 0x2000) in
  let len1000000 = BigInteger.bigInt (`Int 0x1000000) in
  let len80000000 = BigInteger.bigInt (`Int 0x80000000) in
  let lenOr =
    if BigInteger.lesser elen (`BigInt len40) then
      BigInteger.bigInt (`Int 0x80)
    else if BigInteger.lesser elen (`BigInt len2000) then
      BigInteger.bigInt (`Int 0xc000)
    else if BigInteger.lesser elen (`BigInt len1000000) then
      BigInteger.bigInt (`Int 0xe0000000)
    else if BigInteger.lesser elen (`BigInt len80000000) then
      BigInteger.bigIntBaseN (`String "0xf0000000") (`Int 16)
    else
      BigInteger.bigIntBaseN (`String "0xf80000000000") (`Int 16)
  in
  (BigInteger.toString (BigInteger.plus elen (`BigInt lenOr)) ~base:16 ()) ^ padded

let encode_integer_value v =
  let bi = BigInteger.bigIntBaseN (`String v) (`Int 16) in
  if BigInteger.greater bi (`Int 0x7f) then
    encode_hex_digit_list bi
  else
    Printf.sprintf "%02x" (BigInteger.toJSNumber bi)

let encode_string_to_bigint v =
  let enc_array =
    Array.init (String.length v)
      (fun i ->
         let chi = Char.code (String.get v i) in
         Printf.sprintf "%02x" chi
      )
  in
  String.concat "" (Array.to_list enc_array)

let encode_int_to_bigint v =
  BigInteger.toString (BigInteger.bigInt (`String v)) ~base:16 ()

let rec encode : 'a sexp -> string = function
  | Nil _ -> "80"
  | Cons (_,a,b) -> "ff" ^ (encode a) ^ (encode b)
  | Integer (_,v) -> encode_integer_value v
  | Atom (_,v) ->
    encode_integer_value @@ encode_string_to_bigint v
  | QuotedString (_,_,v) ->
    encode_integer_value @@ encode_string_to_bigint v

let intval v =
  BigInteger.toJSNumber (BigInteger.bigInt (`String v))

let sexp_to_bigint = function
  | Nil _ ->
    Some (BigInteger.bigInt (`Int 0))
  | Integer (_,v) ->
    Some (BigInteger.bigInt (`String v))
  | Atom (_,v) ->
    Some (BigInteger.bigIntBaseN (`String (encode_string_to_bigint v)) (`Int 16))
  | QuotedString (_,_,v) ->
    Some (BigInteger.bigIntBaseN (`String (encode_string_to_bigint v)) (`Int 16))
  | _ -> None

let rec equal a b =
  if nilp a && nilp b then
    true
  else if nilp a || nilp b then
    false
  else
    match (a,b) with
    | (Cons (_,r,s), Cons (_,t,u)) ->
      equal r t && equal s u
    | (Cons (_,_,_), _) -> false
    | (_, Cons (_,_,_)) -> false
    | (Integer (_,a),Integer (_,b)) -> a == b
    | (a,b) -> encode a == encode b
