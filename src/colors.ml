open Graphics

let grey = rgb 160 160 160

let current = ref black
let previous = ref None

let set_color c = set_color c ; previous := Some !current ; current := c
let revert_color () = match !previous with
  | Some c -> set_color c ; previous := None
  | None -> ()
