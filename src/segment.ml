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

let get_z p s = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) 

let get_position p s =
     let z = get_z p s
     in if z > 0 then L
     else if z=0 then C
     else R;;

let split_segment d1 s left = 
    let d = (d1.pdest.x - d1.porig.x) * (s.pdest.y - s.porig.y) - (d1.pdest.y - 
            d1.porig.y) * (s.pdest.x - s.porig.x)
    in let r = ((d1.porig.y - s.porig.y) * (s.pdest.x - s.porig.x) - (d1.porig.x -
                s.porig.c) * (s.pdest.y - s.porig.y)) / d
    in let s = ((
    in if d=0 || c < 0 || c >= 1 then if left then (Some s,None) else (None,Some s)
    else if c > 0 && c < 1 then if left then (*séparer*) else (*séparer reverse*)
    else if left then split_segment

    

let split hd rest = failwith "TODO"
