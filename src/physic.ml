open Bsp
open Segment
(* !!!!! Cette fonction est fausse, il ne faut pas seulement vérifier si la position est en conflit
 * avec un segment mais si le disque de rayon step_dist et de centre p a une intersection avec un segment
 * sinon on peut se tp*)
let detect_collision p bsp = 
  let rec findcollision bsp = match bsp with
    | E -> false
    | N(s,lt,rt) -> match get_position p s with
                      | L -> findcollision lt
                      | R -> findcollision rt
                      | C -> true
   in findcollision bsp
