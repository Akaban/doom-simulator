open Graphics
open Segment
open Trigo
open Player
open Point
open Options
open Colors

exception NePasTraiter

let fill_background color =
  set_color Options.bg;
  fill_rect 0 0 win_w win_h;
  revert_color ()

let drawSegment s =
  let (xo,yo),(xd,yd) = real_coordInt s in
  Graphics.draw_segments [|xo,yo,xd,yd|]

let drawSegmentScale scale s = drawSegment {s with porig={s.porig with x=s.porig.x/scale ; y=s.porig.y/scale};
                                                   pdest={s.pdest with x=s.pdest.x/scale ; y=s.pdest.y/scale}}


let rotateSegment rs p =
  let s = !rs in 
      let xo = float_of_int (s.porig.x - p.pos.x) *. dcos (-p.pa) -. float_of_int (s.porig.y - p.pos.y) *. dsin (-p.pa) in
      let yo = float_of_int (s.porig.y - p.pos.y) *. dcos (-p.pa) +. float_of_int (s.porig.x - p.pos.x) *. dsin (-p.pa) in
      let xd = float_of_int (s.pdest.x - p.pos.x) *. dcos (-p.pa) -. float_of_int (s.pdest.y - p.pos.y) *. dsin (-p.pa) in
      let yd = float_of_int (s.pdest.y - p.pos.y) *. dcos (-p.pa) +. float_of_int (s.pdest.x - p.pos.x) *. dsin (-p.pa) in
      rs := new_segmentSimple (truncate xo) (truncate yo) (truncate xd) (truncate yd) 

let clipSegment rs p = 
  if  !rs.porig.x < 1 && !rs.pdest.x < 1 then raise NePasTraiter
  else if !rs.porig.x < 1 then rs := new_segment 1 
          (!rs.porig.y + (1 - !rs.porig.x) * truncate (tangle !rs))
          !rs.pdest.x !rs.pdest.y
  else if !rs.pdest.x < 1 then rs := new_segmentSimple !rs.porig.x !rs.porig.y 1 
            (truncate (float_of_int (!rs.pdest.y) +. (float_of_int (1 - !rs.pdest.x)) *. (tangle !rs)));
            if !rs.porig.x > max_dist then rs := new_segmentSimple max_dist !rs.porig.y !rs.pdest.x !rs.pdest.y ;
          if !rs.pdest.x > max_dist then rs:= new_segmentSimple !rs.porig.x !rs.porig.y max_dist !rs.pdest.y

(*Projette le segment sur l'écran et renvoie les sommets du polygone*)
let projectionSegment rs =
  let d_focale = truncate ((float_of_int win_w /. 2.) /. (dtan (fov /2))) in
  let (xo,yo),(xd,yd) = Segment.real_coordInt !rs in
  let nyo,nyd = (win_w / 2)  - (yo * d_focale / xo), (win_w / 2)  - (yd * d_focale / xd)  in
  if nyo < 0 && nyd < 0 || nyo > win_w && nyd > win_w then raise NePasTraiter
  else let hsDiv = win_h / 2 in
  let zc x = hsDiv + ((ceiling_h - wall_h) * d_focale) / x in
  let zf x = hsDiv + ((floor_h - wall_h) * d_focale) / x in
  let zco, zfo, zcd, zfd = zc xo, zf xo, zc xd, zf xd
  in (*Format.eprintf "Les sommets de projection sont (%d, %d), (%d,%d), (%d,%d), (%d,%d)\n" nyo zco nyd zcd nyo zfo nyd zfd ;*)
     flush stdout;
     [|nyo,zco; nyo, zfo ; nyd, zfd; nyd, zcd|] 

let display bsp p =
  let parseFunction2d = drawSegment in
  let parseMiniMap = drawSegmentScale Options.scale in
  let not_zero x = if x <= 1 then 1 else x in
  let parseFunction3d s = 
    let segment = ref s in
    try
      let () = rotateSegment segment p ; clipSegment segment p in
      let vertices = projectionSegment segment in
      (*draw_segments [|nyo, zco, nyo, zfo|];
           draw_segments [|nyd, zcd, nyd, zfd|];*)
      fill_poly vertices
    with NePasTraiter -> ()
      in match mode with
        | TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y size2d ; set_color black
        | ThreeD -> clear_graph () ; fill_background Options.bg ; Bsp.parse parseFunction3d bsp (p.pos) ; 
                    set_color white ; Bsp.parse parseMiniMap bsp (p.pos); revert_color () ; set_color red ;
                    fill_circle (p.pos.x / scale) (p.pos.y/scale) (not_zero (size2d/scale)) ; revert_color ()

