open Options
open Physic
open Point
open Segment

type t = {
  mutable pos : Point.t;
  mutable pa : int;
  mutable oldpos : Point.t
}

let new_player pos pa = { pos=pos;pa=pa;oldpos=pos}

type dir = Left | Right

let rotate d p = match d with
  | Left -> p.pa <- p.pa - mouse_sensitivity mod 360
  | Right -> p.pa <- p.pa + mouse_sensitivity mod 360

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
                  | MFwd -> 0. , step_dist
                  | MBwd -> 0. , -.(step_dist)
                  | MLeft -> -.(step_dist), 0.
                  | MRight -> step_dist, 0.
               in let new_pos = new_point (int_of_float ((float_of_int p.pos.x +. dx) *. Trigo.dcos p.pa)) 
                                (int_of_float ((float_of_int p.pos.y +. dy) *. Trigo.dsin p.pa))
               in match (detect_collision new_pos bsp) with
                  | Some s -> ()
                  | None -> p.pos <- new_pos
                 
              
