open Options
open Physic
open Point

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

let move d p bsp = match mode with
  | TwoD -> begin
            let step = truncate step_dist in
            p.oldpos <- p.pos ;
            match d with
              | MFwd -> p.pos <- new_point p.pos.x (p.pos.y + step)
              | MBwd -> p.pos <- new_point p.pos.x (p.pos.y - step)
              | MLeft -> p.pos <- new_point (p.pos.x - step) p.pos.y
              | MRight -> p.pos <- new_point (p.pos.x + step) p.pos.y (*ajouter pour la 2d la detection de collision*)
            end
  | ThreeD -> begin
              let dx, dy =
                match d with
                  | MFwd -> 0. , step_dist
                  | MBwd -> 0. , -.(step_dist)
                  | MLeft -> -.(step_dist), 0.
                  | MRight -> step_dist, 0.
               in let new_pos = new_point (int_of_float ((float_of_int p.pos.x +. dx) *. Trigo.dcos p.pa)) 
                                (int_of_float ((float_of_int p.pos.y +. dy) *. Trigo.dsin p.pa))
               in if (not (detect_collision new_pos bsp)) then
                    p.pos <- new_pos
              end
