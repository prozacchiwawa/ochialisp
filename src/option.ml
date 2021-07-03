let map f = function
  | None -> None
  | Some a -> Some (f a)

let bind f = function
  | None -> None
  | Some a -> f a
