open Point

type t = { id : int ;
          porig : Point.t; 
          pdest : Point.t;
          ci : float;
          ce : float;
          segBottom : t option;
          segRight : t option;
          segTop : t option;
          segLeft : t option
         }

let compteur x = let cpt = ref x in fun () -> cpt := !cpt + 1 ; !cpt;;
let idCount = compteur 0;;

let fromSome default = function
  | Some s -> s
  | _ -> default

type tpos = L | R | C

let tposToString = function
  | L -> "Left"
  | R -> "Right"
  | C -> "Center"

let tangle s = (*tangente de l'angle du segment*)
  let a = float_of_int (s.pdest.y - s.porig.y)
  in let b = float_of_int (s.pdest.x - s.porig.y)
  in a /. b

let angle s = (*angle du segment*)
 Trigo.datan (tangle s)

let angleWithPoint p1 p2 =
  let a = float_of_int (p2.y - p1.y)
  in let b = float_of_int (p2.x - p1.x)
  in Trigo.datan (a/. b)


let toString s = Printf.sprintf "{id=%d;porig=%s;pdest=%s;ci=%f;ce=%f;angle=%d}" s.id (toString s.porig) (toString s.pdest) s.ci s.ce (truncate (angle s)) 

let to_f = float_of_int
let to_i = int_of_float

let new_segmentPointSimple p1 p2 = { id=(-1) ; porig=p1 ; pdest=p2 ; ci = 0.0 ; ce = 1.0 ; segBottom = None
                            ; segRight = None ; segTop = None ; segLeft = None}

let translateVect (dx,dy) alpha = new_point (to_i (dx *. Trigo.dcos alpha -. dy *. Trigo.dsin alpha)) 
                                               (to_i ( dx *. Trigo.dsin alpha +. dy *. Trigo.dcos alpha))
let translatePoint p vectPoint = new_point (p.x + vectPoint.x) (p.y + vectPoint.y)

let sgn x = if x < 0 then -1 else 1

let new_segmentPoint p1 p2 = let idc = idCount () in
                             let angle1 = truncate (angleWithPoint p1 p2) in
                             let angle = 180 - angle1 mod 90 in
                             let step_dist = Options.step_dist in
                             let bottomRight = translatePoint p1 (translateVect (step_dist,0.) angle)
                             in let bottomLeft = translatePoint p1 (translateVect (step_dist,0.) (angle+180)) in
                             let topRight = translatePoint p2 (translateVect (step_dist,0.) angle) in
                             let topLeft = translatePoint p2 (translateVect (step_dist,0.) (angle+180)) 
                             in { id=idc ; porig=p1 ; pdest=p2 ; ci = 0.0 ; ce = 1.0; 
                                segBottom = Some (new_segmentPointSimple bottomLeft bottomRight);
                                segLeft = Some (new_segmentPointSimple bottomLeft topLeft);
                                segTop = Some (new_segmentPointSimple topLeft topRight);
                                segRight = Some (new_segmentPointSimple bottomRight topRight) }

let new_segment xo yo xd yd = new_segmentPoint (new_point xo yo) (new_point xd yd) 

let real_coord s =
  let lx = s.pdest.x - s.porig.x
  in let ly = s.pdest.y - s.porig.y
  in let (xo,yo) = ( float_of_int s.porig.x +. (float_of_int lx) *. s.ci, float_of_int s.porig.y +. (float_of_int ly) *. s.ci)
  in let (xd,yd) = ( float_of_int s.porig.x +. (float_of_int lx) *. s.ce, float_of_int s.porig.y +. (float_of_int ly) *. s.ce)
  in (xo,yo),(xd,yd)

let real_coordInt s =
  let (xo,yo),(xd,yd) = real_coord s in
  (truncate xo,truncate yo),(truncate xd,truncate yd)

let bottomRight s = match s.segRight with
  | Some s -> let (x,y),_ = real_coord s in new_point (truncate x) (truncate y)
  | None -> raise Not_found

let drawSegment s =
  let (xo,yo),(xd,yd) = real_coord s in
  Graphics.draw_segments [|(truncate xo,truncate yo,truncate xd,truncate yd)|]

let drawCollisionZone s =
  if s.segBottom = None then () 
  else let getSome = fromSome s in
  let segs = [getSome s.segBottom; getSome s.segLeft; getSome s.segRight; getSome s.segTop]
  in List.iter drawSegment segs

let get_z p s = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) 

let originVector s = (s.pdest.x - s.porig.x,s.pdest.y - s.porig.y)

let get_position p s =
     let z = get_z p s
     in if z > 0 then L
     else R;;

let get_position_s p s =
  let pos = get_position p s in
  tposToString pos

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
    | Some p -> if p=0. then begin
                match (get_position s.pdest d) with
                  | L -> (Some s,None)
                  | _ -> (None,Some s)
                  end else
                let s1 = {s with ce = p;
                                id = idCount ()}
                in let s2 = {s with ci=p;
                                id = idCount ()}
                in let (s1xo,s1yo),(s1xd,s1yd) = real_coordInt s1 in
                let (s2xo,s2yo),(s2xd,s2yd) = real_coordInt s2 in
                let ts1 = new_segment s1xo s1yo s1xd s1yd
                in let ts2 = new_segment s2xo s2yo s2xd s2yd
                in let (rs1,rs2) = { ts1 with ce=p ; id=ts1.id ; porig=s1.porig ; pdest = s1.pdest}, { ts2 with ci=p ; id=ts2.id ;
                   porig=s2.porig; pdest=s2.pdest}
                in begin match (get_position s.porig d) with
                         | L -> (Some rs1,Some rs2)
                         | _ -> (Some rs2,Some rs1)
                   end;;
    
let split hd r =
  let rec split_do rest (l,r) = 
    match rest with
      | (t::ts) -> begin
            match (split_segment hd t) with
             | (Some s,None) -> split_do ts (s::l,r)
             | (None,Some s) -> split_do ts (l,s::r)
             | (Some s1,Some s2) -> split_do ts (s1::l,s2::r)
             | (None,None) -> assert false
             end
      | [] -> (l,r)
   in split_do r ([],[]);;
 
