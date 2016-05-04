open Bsp
open Segment

let detect_collision p bsp = 
  let rec findcollision bsp = match bsp with
    | E -> false
    | N(s,lt,rt) -> match get_position p s with
                      | L -> findcollision lt
                      | R -> findcollision rt
                      | C -> true
   in findcollision bsp
