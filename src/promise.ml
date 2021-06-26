(* tea_promise.ml *)
let cmd promise tagger =
  let open Vdom in
  Tea_cmd.call (function callbacks ->
      let _ = promise
              |> Js.Promise.then_ (function res ->
                match tagger res with
                | Some msg ->
                  let () = !callbacks.enqueue msg in
                  Js.Promise.resolve ()
                | None -> Js.Promise.resolve ()
                )
      in
      ()
    )
