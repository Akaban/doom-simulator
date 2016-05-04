open Bsp
open Segment
(* !!!!! Cette fonction est fausse, il ne faut pas seulement vérifier si la position est en conflit
 * avec un segment mais si le disque de rayon step_dist et de centre p a une intersection avec un segment
 * sinon on peut se tp*)
let detect_collision p bsp =
  let rec findcollision bsp = 
    match bsp with
    | E -> None
    | N(s,lt,rt) -> let getSeg = fromSome s in
                    match get_position p s with
                      | R -> if (get_position p (getSeg s.segRight) = L && 
                        get_position p (getSeg s.segBottom) = L && get_position p (getSeg s.segTop) = R) then Some s else findcollision rt
                      | L -> if (get_position p (getSeg s.segLeft) = R && get_position p (getSeg s.segBottom) = L && 
                             get_position p (getSeg s.segTop) = R) then Some s else findcollision lt
                      | C -> Some s
   in findcollision bsp
