open Graphics
open Segment
open Trigo
open Player
open Point

let display bsp p =
  let rotateSegment s =
      let xo = float_of_int (s.porig.x - p.pos.x) *. dcos (-(p.pa)) -. float_of_int (s.porig.y - p.pos.y) *. dsin (-(p.pa)) in
      let yo = float_of_int (s.porig.y - p.pos.y) *. dcos (-p.pa) -. float_of_int (s.porig.x - p.pos.x) *. dsin (-p.pa) in
      let xd = float_of_int (s.pdest.x - p.pos.x) *. dcos (-(p.pa)) -. float_of_int (s.pdest.y - p.pos.y) *. dsin (-p.pa) in
      let yd = float_of_int (s.pdest.y - p.pos.y) *. dcos (-(p.pa)) -. float_of_int (s.pdest.x - p.pos.x) *. dsin (-p.pa) in
      new_segment (truncate xo) (truncate yo) (truncate xd) (truncate yd) 
      in let parseFunction2d s =
           let rotated = s in
           let rs = ref rotated in
           let (xo,yo),(xd,yd) = real_coord !rs in
           draw_segments [|(truncate xo,truncate yo,truncate xd,truncate yd)|]
    in 
    let parseFunction3d s = failwith "TODO parse3D"
    in match Options.mode with
        | Options.TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y Options.size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y Options.size2d ; set_color black
        | Options.ThreeD -> failwith "TODO 3D"

