type t =
  { line : int
  ; col : int
  ; until : (int * int) option
  }

let srcLocationToJson sl =
  let b =
    [ ("line", Js.Json.number (float_of_int sl.line))
    ; ("col", Js.Json.number (float_of_int sl.col))
    ]
  in
  let u =
    match sl.until with
    | None -> []
    | Some (l,c) ->
      [ ("ml", Js.Json.number (float_of_int l))
      ; ("mc", Js.Json.number (float_of_int c))
      ]
  in
  List.concat [ b ; u ]
  |> Js.Dict.fromList
  |> Js.Json.object_

let toString a =
  match a.until with
  | None -> Printf.sprintf "%d:%d" a.line a.col
  | Some (l,c) -> Printf.sprintf "%d:%d-%d:%d" a.line a.col l c

let srcLocationMin a = (a.line, a.col)
let srcLocationMax a =
  match a.until with
  | None -> (a.line, a.col + 1)
  | Some (ll,cc) -> (ll,cc)

let fromPair (l,c) = { line = l ; col = c ; until = None }

let combineSrcLocation a b =
  let addOnto x y = { x with until = Some (srcLocationMax y) } in
  if a.line < b.line then
    addOnto a b
  else if a.line == b.line then
    if a.col < b.col then
      addOnto a b
    else if a.col == b.col then
      a
    else
      addOnto b a
  else
    addOnto b a

let start = { line = 1 ; col = 1 ; until = None }
let advance loc =
  function
  | '\n' -> { loc with col = 1 ; line = loc.line + 1 }
  | '\t' ->
    let nextTab = (loc.col + 8) land (lnot 7) in
    { loc with col = nextTab }
  | _ -> { loc with col = loc.col + 1 }
