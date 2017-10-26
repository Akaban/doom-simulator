(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Bryce Tichit *)
open Bsp
open Segment

let hardFromSome = function
    | Some s -> s
    | None -> failwith "hardFromSome with None"


(*let leftCollision = (R,L)*)
let rightCollision = (L,R)
let leftCollision = rightCollision
(*La position des segments de collision est différente selon le sens de notre segment
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
                             get_position p (getSeg s.segTop) = cdirL) then (Some s) else (findcollision lt)
                      | _ -> if (get_position p (getSeg s.segLeft) = cdirL && get_position p (getSeg s.segRight) = cdirR && 
                                get_position p (getSeg s.segBottom) = cdirR && 
                                get_position p (getSeg s.segTop) = cdirL) then (Some s) else (findcollision rt)
  in if !Options.collision then findcollision bsp else None (*le mode debug permet d'enlever les collision [noclip]*)
