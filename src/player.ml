open Options
open Physic

type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = { pos=pos;pa=pa}

type dir = Left | Right

let rotate d p = match d with
  | Left -> p.pa <- p.pa - mouse_sensitivity
  | Right -> p.pa <- p.pa + mouse_sensitivity

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp = failwith "TODO"
