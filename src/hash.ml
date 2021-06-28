type wrapper
type t

external sha256module : wrapper = "sha256" [@@bs.module "js-sha256"]
external create_ : wrapper -> t = "create" [@@bs.send]
let create _ = create_ sha256module
external update : t -> string -> unit = "update" [@@bs.send]
external hex : t -> string = "hex" [@@bs.send]

