open Sexp
open Runtypes

let do_arith l op a b =
  let converted_args =
    sexp_to_bigint a
    |> Option.bind
      (fun a ->
         sexp_to_bigint b
         |> Option.map (fun b -> (a,b))
      )
  in
  match converted_args with
  | None ->
    RunError
      (l, "bad argument conversion for " ^ to_string a ^ " and " ^ to_string b)
  | Some (a,b) ->
    let result = op a b in
    RunOk (Integer (l,BigInteger.toString result ~base:10 ()))

let add a b =
  BigInteger.add a (`BigInt b)

let subtract a b = BigInteger.subtract a (`BigInt b)

let multiply a b = BigInteger.multiply a (`BigInt b)

let divide a b = BigInteger.divide a (`BigInt b)

let do_greater a b =
  if BigInteger.greater a (`BigInt b) then
    BigInteger.bigInt (`Int 1)
  else
    BigInteger.bigInt (`Int 0)

let do_divmod l _a _b = RunError (l, "not implemented")

let ash v n =
  BigInteger.shiftLeft v (BigInteger.toJSNumber n)

let lsh v n =
  BigInteger.shiftLeft v (BigInteger.toJSNumber n)

let logand a b = BigInteger.and_ a (`BigInt b)

let logior a b = BigInteger.or_ a (`BigInt b)

let logxor a b = BigInteger.xor a (`BigInt b)
