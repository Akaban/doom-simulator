open Bsp
open Segment

let hardFromSome = function
    | Some s -> s
    | None -> failwith "hardFromSome with None"

(*Calcule les positions du joueur par rapport aux segments de la zone de collision
 * utilisée pour le debug uniquement et ne prends pas en compte
 * le sens du segment*)
let collisionPlayer p s =
  let sTop, sRight, sBot, sLeft = 
        hardFromSome s.segTop, hardFromSome s.segRight,
        hardFromSome s.segBottom , hardFromSome s.segLeft
  in let [pTop;pRight;pBot;pLeft] = List.map (get_position p) 
                                    [sTop;sRight;sBot;sLeft]
  in (pTop,pRight,pBot,pLeft)

let leftCollision = (R,L)
let rightCollision = (L,R)
(*La position des segments de collision est différente selon le sens de notre segment
 * il convient donc de distinguer celui-ci *)


let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = hardFromSome in
    let foundCol () = Printf.printf "Found a collision with %d\n" s.id ; flush stdout in
    let noCol d = Printf.printf "Did not found a collision with %d go %s\n" s.id d ; flush stdout in 
                    let (cdirL,cdirR) = if s.sens = L then leftCollision else rightCollision in 
                    match get_position p s with
                      | L -> if (get_position p (getSeg s.segRight) = cdirR && get_position p (getSeg s.segLeft) = cdirL && 
                              get_position p (getSeg s.segBottom) = cdirR && 
                             get_position p (getSeg s.segTop) = cdirL) then (Some s) else (findcollision lt)
                      | _ -> if (get_position p (getSeg s.segLeft) = cdirL && get_position p (getSeg s.segRight) = cdirR && 
                                get_position p (getSeg s.segBottom) = cdirR && 
                                get_position p (getSeg s.segTop) = cdirL) then (Some s) else (findcollision rt)
  in if !Options.collision then findcollision bsp else None (*le mode debug permet d'enlever les collision [noclip]*)
