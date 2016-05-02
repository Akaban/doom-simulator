open Point

type t = { id : int ;
          porig : Point.t; 
          pdest : Point.t;
          ci : float;
          ce : float
         }

let compteur x = let cpt = ref x in fun () -> cpt := !cpt + 1 ; !cpt;;
let idCount = compteur 0;;

type tpos = L | R | C

let new_segment xo yo xd yd = let idc = idCount () in { id=idc ; porig=Point.new_point xo yo; pdest=Point.new_point xd yd; ci=0.0;ce=1.0 } 

let get_position p s = 
     let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x)
     in if z > 0 then L
     else if z=0 then C
     else R;;

let split_segment d s = failwith "TODO"

let split hd rest = failwith "TODO"
