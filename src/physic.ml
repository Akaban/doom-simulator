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



let leftCollision = (R,L)
let rightCollision = (L,R)
(*La position des segments de colision est différente selon le sens de notre segment
 * il convient donc de distinguer celui-ci *)


let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = hardFromSome in
                    let (cdirL,cdirR) = if s.sens = L then leftCollision else rightCollision in 
                    match get_position p s with
                      | L -> if (get_position p (getSeg s.segRight) = cdirR && get_position p (getSeg s.segLeft) = cdirL && 
                              get_position p (getSeg s.segBottom) = cdirR && 
                             get_position p (getSeg s.segTop) = cdirL) then Some s else findcollision lt
                      | _ -> if (get_position p (getSeg s.segLeft) = cdirL && get_position p (getSeg s.segRight) = cdirR && 
                                get_position p (getSeg s.segBottom) = cdirR && 
                                get_position p (getSeg s.segTop) = cdirL) then Some s else findcollision rt
   in if !Options.collision then findcollision bsp else None
