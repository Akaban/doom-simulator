open Options
open Physic
open Point
open Segment

type t = {
  mutable pos : Point.t;
  mutable pa : int;
  mutable oldpos : Point.t;
  mutable rAngleMinimap : Segment.t;
  mutable lAngleMinimap : Segment.t
}

let calculateAngleMinimap p pa =
let divPos = divPoint p scale in
  let rightAnglePos = translatePointWithAngle divPos (sizeAngleMiniMap,0.) (pa-angleMiniMap) in
  let leftAnglePos = translatePointWithAngle divPos (sizeAngleMiniMap,0.) (pa+angleMiniMap) in
  new_segmentPointSimple divPos rightAnglePos, new_segmentPointSimple divPos leftAnglePos


let new_player pos pa =
  let rMinimap,lMinimap = calculateAngleMinimap pos pa in
  { pos=pos;pa=pa;oldpos=pos;
    rAngleMinimap = rMinimap;
    lAngleMinimap = lMinimap}
                          
                          

type dir = Left | Right

let rotate d p = match d with
  | Left -> p.pa <- p.pa + angular_change mod 360 ;
            let lMinimap, rMinimap = calculateAngleMinimap p.pos p.pa in 
            p.rAngleMinimap <- lMinimap; p.lAngleMinimap <- rMinimap 
  | Right -> p.pa <- p.pa - angular_change mod 360 ;
             let lMinimap, rMinimap = calculateAngleMinimap p.pos p.pa in 
             p.rAngleMinimap <- lMinimap; p.lAngleMinimap <- rMinimap 
 

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp = 
  match mode with
  | TwoD -> let step = truncate step_dist in
            let dx, dy = 
              match d with
                  | MFwd -> 0 , step
                  | MBwd -> 0 , -step
                  | MLeft -> -step, 0
                  | MRight -> step, 0
            in let new_pos = new_point (p.pos.x + dx) (p.pos.y + dy)
            in begin match (detect_collision new_pos bsp) with
                | Some s -> if !debug then begin
                          let (Some segt) = s.segTop in
                          print_string ("Collision detecte avec " ^ Segment.toString s ^ "\n"); 
                          flush stdout ; Printf.printf "Votre position par rapport au segTop est %s et par rapport au seg est %s\n" 
                          (Segment.get_position_s new_pos segt) (Segment.get_position_s new_pos s);
                          flush stdout
                          end
                | None -> p.oldpos <- p.pos ; p.pos <- new_pos
            end
          
  | ThreeD -> let dx, dy =
                match d with
                  | MLeft -> 0. , step_dist
                  | MRight -> 0. , -.(step_dist)
                  | MBwd -> -.(step_dist), 0.
                  | MFwd -> step_dist, 0.
               in let new_pos = translatePointWithAngle p.pos (dx,dy) p.pa
               in match (detect_collision new_pos bsp) with
                  | Some s -> ()
                  | None -> p.pos <- new_pos ; 
                  let lMinimap, rMinimap = calculateAngleMinimap new_pos p.pa in
                  p.rAngleMinimap <- lMinimap ; p.lAngleMinimap <- rMinimap
                 
              
