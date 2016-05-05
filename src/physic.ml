open Bsp
open Segment

let hardFromSome = function
    | Some s -> s
    | None -> failwith "hardFromSome with None"

let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = hardFromSome in
                    match get_position p s with
                      | L -> if (get_position p (getSeg s.segLeft) = R && get_position p (getSeg s.segBottom) = L && 
                             get_position p (getSeg s.segTop) = R) then Some s else findcollision lt
                      | _ -> if (get_position p (getSeg s.segRight) = L && get_position p (getSeg s.segBottom) = L && 
                                get_position p (getSeg s.segTop) = R) then Some s else findcollision rt
   in findcollision bsp
