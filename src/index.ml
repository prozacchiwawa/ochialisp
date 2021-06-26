open Tea.App
open Tea.Html
open Sexp

type msg
  = Noop
  | InputSexp of string
  [@@bs.deriving {accessors}]

type model =
  { input_sexp : string
  ; parsed_sexp : (Srcloc.t sexp) option
  }

let init () =
  ( { input_sexp = ""
    ; parsed_sexp = None
    }
  , Tea.Cmd.none
  )

let update model = function
  | Noop -> (model, Tea.Cmd.none)
  | InputSexp v -> ({ model with input_sexp = v }, Tea.Cmd.none)

let view model =
  let parse_result =
    parse_sexp Srcloc.combineSrcLocation Srcloc.start Srcloc.advance model.input_sexp
  in
  let show_result =
    match parse_result with
    | Success exprs ->
      let tojson =
        List.map (sexp_to_json Srcloc.srcLocationToJson) exprs
        |> Array.of_list
        |> Js.Json.array
      in
      div [] [text (Js.Json.stringify tojson)]
    | Failure (l,e) ->
      div []
        [ text e
        ; text " @ "
        ; text (string_of_int l.line)
        ; text ","
        ; text (string_of_int l.col)
        ]
  in
  div
    []
    [ textarea
        [ Vdom.attribute "" "rows" "30"
        ; Vdom.attribute "" "cols" "80"
        ; onInput (fun a -> InputSexp a) ; value model.input_sexp
        ] []
    ; show_result
    ]

let main =
  standardProgram { (* The beginnerProgram just takes a set model state and the update and view functions *)
    init;
    update;
    view;
    subscriptions = fun _ -> Tea.Sub.none;
  }

let _ =
  main (Web.Document.getElementById "app") ()
