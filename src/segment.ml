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

let new_segment xo yo xd yd = let idc = idCount () in { id=idc ; porig=Point.new_point xo yo; pdest=Point.new_point xd yd; ci= 0.0 ; ce = 1.0} 
let new_segmentPoint p1 p2 = let idc = idCount () in { id=idc ; porig=p1 ; pdest=p2 ; ci = 0.0 ; ce = 1.0}

let real_coord s =
  let lx = s.pdest.x - s.porig.x
  in let ly = s.pdest.y - s.porig.y
  in let (xo,yo) = ( float_of_int s.porig.x +. (float_of_int lx) *. s.ci, float_of_int s.porig.y +. (float_of_int ly) *. s.ci)
  in let (xd,yd) = ( float_of_int s.porig.x +. (float_of_int lx) *. s.ce, float_of_int s.porig.y +. (float_of_int ly) *. s.ce)
  in ((xo,yo),(xd,yd))

let get_z p s = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) 

let originVector s = (s.pdest.x - s.porig.x,s.pdest.y - s.porig.y)

let get_position p s =
     let z = get_z p s
     in if z > 0 then L
     else if z=0 then C
     else R;;


(*les deux fonction suivantes sont à vérifier, en particulier pour l'intersection*)
let coordInterception d s =
    let (osx,osy) = originVector s
    in let (odx,ody) = originVector d
    in if osx=0 && odx=0 then None
    else let cd = (d.pdest.x - d.porig.x) * (s.pdest.y - s.porig.y) - (d.pdest.y - 
                 d.porig.y) * (s.pdest.x - s.porig.x)
         in if cd=0 then None else
         let cr = float_of_int ((d.porig.y - s.porig.y) * (s.pdest.x - s.porig.x) - (d.porig.x -
                         s.porig.x) * (s.pdest.y - s.porig.y)) /. (float_of_int cd)
         in let cs = float_of_int ((d.porig.y - s.porig.y) * (d.pdest.x - d.porig.x) - (d.porig.x
                      - s.porig.x) * (d.pdest.y - d.porig.y)) /. (float_of_int cd)
         in if cs >= s.ce || cs < s.ci then None
               else Some cs;; 

     
let split_segment d s =
   match (coordInterception d s) with
    | None -> begin match (get_position s.porig d) with
                | L -> (Some s,None)
                | _ -> (None,Some s)
              end
    | Some p -> let s1 = {s with ce = p;
                                id = idCount ()}
                in let s2 = {s with ci=p;
                                id = idCount ()}
                in begin match (get_position s.porig d) with
                         | L -> (Some s1,Some s2)
                         | _ -> (Some s2,Some s1)
                   end;;
    
let split hd r =
  let rec split_do rest (l,r) = 
    match rest with
      | (t::ts) -> begin
            match (split_segment hd t) with
             | (Some s,None) -> split_do ts (s::l,r)
             | (None,Some s) -> split_do ts (l,s::r)
             | (Some s1,Some s2) -> split_do ts (s1::l,s2::r)
             | _ -> assert false
             end
      | [] -> (l,r)
   in split_do r ([],[]);;
