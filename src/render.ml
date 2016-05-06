open Graphics
open Segment
open Trigo
open Player
open Point
open Options


let display bsp p =
  let rotateSegment s =
      let xo = float_of_int (s.porig.x - p.pos.x) *. dcos (-p.pa) -. float_of_int (s.porig.y - p.pos.y) *. dsin (-p.pa) in
      let yo = float_of_int (s.porig.y - p.pos.y) *. dcos (-p.pa) +. float_of_int (s.porig.x - p.pos.x) *. dsin (-p.pa) in
      let xd = float_of_int (s.pdest.x - p.pos.x) *. dcos (-p.pa) -. float_of_int (s.pdest.y - p.pos.y) *. dsin (-p.pa) in
      let yd = float_of_int (s.pdest.y - p.pos.y) *. dcos (-p.pa) +. float_of_int (s.pdest.x - p.pos.x) *. dsin (-p.pa) in
      new_segmentSimple (truncate xo) (truncate yo) (truncate xd) (truncate yd) 
      in let parseFunction2d = drawSegment
    in 
    let parseFunction3d s = 
      let rotated = rotateSegment s in
          let rs = ref rotated in
          if  !rs.porig.x < 1 && !rs.pdest.x < 1 then ()
          else if !rs.porig.x < 1 then rs := new_segment 1 
          (truncate (float_of_int (!rs.porig.y) +. float_of_int (1 - !rs.porig.x) *. (tangle !rs)))
          !rs.pdest.x !rs.pdest.y
          else if !rs.pdest.x < 1 then rs := new_segmentSimple !rs.porig.x !rs.porig.y 1 
            (truncate (float_of_int (!rs.pdest.y) +. (float_of_int (1 - !rs.pdest.x)) *. (tangle !rs)))
          ; if !rs.porig.x > max_dist then rs := new_segmentSimple max_dist !rs.porig.y !rs.pdest.x !rs.pdest.y ;
          if !rs.pdest.x > max_dist then rs:= new_segmentSimple !rs.porig.x !rs.porig.y max_dist !rs.pdest.y ;
          let mtan = dtan (fov / 2) in
          let d_focale = (float_of_int win_w /. 2.) /. (dtan (fov /2)) in
          let (xo,yo),(xd,yd) = Segment.real_coord !rs in
          let lsDiv = float_of_int win_w /. 2. in
          let nyo,nyd = lsDiv -. (yo *. d_focale /. xo), lsDiv -. (yd *. d_focale /. xd)  in
          let nyo, nyd = truncate nyo, truncate nyd in
          if nyo <= 0 && nyd <= 0 then ()
          else rs := new_segmentSimple (truncate d_focale) nyo (truncate d_focale) nyd ;
          let hsDiv = float_of_int win_h /. 2. in
          let zc x = hsDiv +. float_of_int (ceiling_h - wall_h) *. d_focale /. x in
          let zf x = hsDiv +. float_of_int (floor_h - wall_h) *. d_focale /. x in
          let zco, zfo, zcd, zfd, d = truncate (zc xo), truncate (zf xo),
            truncate (zc xd), truncate (zf xd), truncate d_focale in
          let vertices = [|nyo, zco; nyd, zfo; nyo, zcd; nyd, zfd|] in
          Format.eprintf "(%d, %d, %d), (%d, %d, %d)@." nyo zco zfo nyd zcd zfd;
          (*draw_segments [|nyo, zco, nyo, zfo|];
          draw_segments [|nyd, zcd, nyd, zfd|];*)
          fill_poly vertices
    in match mode with
        | TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y size2d ; set_color black
        | ThreeD -> Bsp.parse parseFunction3d bsp (p.pos)

