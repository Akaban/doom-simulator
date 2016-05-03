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
    in let doParse = if Options.mode = Options.TwoD then parseFunction2d
                   else parseFunction3d
    in Bsp.parse doParse bsp (p.pos)

