open Bsp
open Segment
let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = fromSome s in
                    match get_position p s with
                      | R -> if (s.ce > 0. && get_position p (getSeg s.segRight) = L && 
                        get_position p (getSeg s.segBottom) = R && get_position p (getSeg s.segTop) = L) then Some s else findcollision rt
                      | L -> if (s.ce > 0. && get_position p (getSeg s.segLeft) = R && get_position p (getSeg s.segBottom) = R && 
                             get_position p (getSeg s.segTop) = L) then Some s else findcollision lt
                      | C -> Some s
   in findcollision bsp
