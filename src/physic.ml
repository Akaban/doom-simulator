open Bsp
open Segment

let hardFromSome = function
    | Some s -> s
    | None -> failwith "hardFromSome with None"

let collisionPlayer p s =
  let sTop, sRight, sBot, sLeft = 
        hardFromSome s.segTop, hardFromSome s.segRight,
        hardFromSome s.segBottom , hardFromSome s.segLeft
  in let [pTop;pRight;pBot;pLeft] = List.map (get_position p) 
                                    [sTop;sRight;sBot;sLeft]
  in (pTop,pRight,pBot,pLeft)


let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = hardFromSome in
                    match get_position p s with
                      | L -> if (get_position p (getSeg s.segRight) = R && get_position p (getSeg s.segLeft) = L && get_position p (getSeg s.segBottom) = R && 
                             get_position p (getSeg s.segTop) = L) then Some s else findcollision lt
                      | _ -> if (get_position p (getSeg s.segLeft) = L && get_position p (getSeg s.segRight) = R && get_position p (getSeg s.segBottom) = R && 
                                get_position p (getSeg s.segTop) = L) then Some s else findcollision rt
   in if !Options.collision then findcollision bsp else None
