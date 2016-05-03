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
  in let doParse s =
          let rotated = rotateSegment s in
          let rs = ref rotated in
          if  !rs.porig.x < 1 && !rs.pdest.x < 1 then ()
          else begin if !rs.porig.x < 1 then rs := new_segment 1 
          (truncate (float_of_int (!rs.porig.y) +. float_of_int (1 - !rs.porig.x) *. (dtan (angle !rs))))
          !rs.pdest.x !rs.pdest.y
          else if !rs.pdest.x < 1 then rs := new_segment !rs.porig.x !rs.porig.y 1 (truncate (float_of_int (!rs.pdest.y) +. (float_of_int (1 - !rs.pdest.x)) *. (dtan (angle !rs))))
          ; if !rs.porig.x > Options.max_dist then rs := new_segment Options.max_dist !rs.porig.y !rs.pdest.x !rs.pdest.y ;
          if !rs.pdest.x > Options.max_dist then rs:= new_segment !rs.porig.x !rs.porig.y Options.max_dist !rs.pdest.y ;
          let (xo,yo),(xd,yd) = real_coord !rs in
          draw_segments [|(truncate xo,truncate yo,truncate xd,truncate yd)|]
          end
    in Bsp.parse doParse bsp (p.pos)
