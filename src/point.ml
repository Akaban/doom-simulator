type t = {x : int; y : int}

let new_point x y = {x=x;y=y}

let toString p = "(x=" ^ string_of_int p.x ^ ",y=" ^ string_of_int p.y ^ ")"
