open Graphics

let grey = rgb 160 160 160
let coral = rgb 83 37 28
let bluesky = rgb 178 229 239

let current = ref black
let previous = ref None

let set_color c = set_color c ; previous := Some !current ; current := c
let revert_color () = match !previous with
  | Some c -> set_color c ; previous := None
  | None -> ()
