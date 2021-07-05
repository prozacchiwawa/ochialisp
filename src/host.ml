type console
external console : console = "console" [@@bs.val]
external trace_ : console -> unit = "trace" [@@bs.send]
let trace _ = trace_ console
