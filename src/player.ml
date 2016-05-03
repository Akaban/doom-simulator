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
  | Left -> p.pa <- p.pa - mouse_sensitivity
  | Right -> p.pa <- p.pa + mouse_sensitivity

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp = match mode with
  | TwoD -> begin
            let step = truncate step_dist in
            p.oldpos <- p.pos ;
            match d with
              | MFwd -> p.pos <- new_point p.pos.x (p.pos.y + step)
              | MBwd -> p.pos <- new_point p.pos.x (p.pos.y - step)
              | MLeft -> p.pos <- new_point (p.pos.x - step) p.pos.y
              | MRight -> p.pos <- new_point (p.pos.x + step) p.pos.y
            end
  | ThreeD -> failwith "todo move3d"
