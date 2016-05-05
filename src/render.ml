open Graphics
open Segment
open Trigo
open Player
open Point


let display bsp p =
  let rotateSegment s =
      let xo = float_of_int (s.porig.x - p.pos.x) *. dcos (-(p.pa)) -. float_of_int (s.porig.y - p.pos.y) *. dsin (-(p.pa)) in
      let yo = float_of_int (s.porig.y - p.pos.y) *. dcos (-p.pa) +. float_of_int (s.porig.x - p.pos.x) *. dsin (-p.pa) in
      let xd = float_of_int (s.pdest.x - p.pos.x) *. dcos (-(p.pa)) -. float_of_int (s.pdest.y - p.pos.y) *. dsin (-p.pa) in
      let yd = float_of_int (s.pdest.y - p.pos.y) *. dcos (-(p.pa)) +. float_of_int (s.pdest.x - p.pos.x) *. dsin (-p.pa) in
      new_segment (truncate xo) (truncate yo) (truncate xd) (truncate yd) 
      in let parseFunction2d = drawSegment
    in 
    let parseFunction3d s = 
      let rotated = rotateSegment s in
          let rs = ref rotated in
          if  !rs.porig.x < 1 && !rs.pdest.x < 1 then ()
          else begin if !rs.porig.x < 1 then rs := new_segment 1 
          (truncate (float_of_int (!rs.porig.y) +. float_of_int (1 - !rs.porig.x) *. (tangle !rs)))
          !rs.pdest.x !rs.pdest.y
          else if !rs.pdest.x < 1 then rs := new_segment !rs.porig.x !rs.porig.y 1 
            (truncate (float_of_int (!rs.pdest.y) +. (float_of_int (1 - !rs.pdest.x)) *. (tangle !rs)))
          ; if !rs.porig.x > Options.max_dist then rs := new_segment Options.max_dist !rs.porig.y !rs.pdest.x !rs.pdest.y ;
          if !rs.pdest.x > Options.max_dist then rs:= new_segment !rs.porig.x !rs.porig.y Options.max_dist !rs.pdest.y ;
          drawSegment !rs
          end
    in match Options.mode with
        | Options.TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y Options.size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y Options.size2d ; set_color black
        | Options.ThreeD -> failwith "TODO 3D"

