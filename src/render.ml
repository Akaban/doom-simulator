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
  let (oxo,oyo),(oxd,oyd) = Segment.real_coord !rs in
  let px, py = float_of_int p.pos.x, float_of_int p.pos.y in
      let xo = (oxo -. px) *. dcos (-p.pa) -. (oyo -. py) *. dsin (-p.pa) in
      let yo = (oyo -. py) *. dcos (-p.pa) +. (oxo -. px) *. dsin (-p.pa) in
      let xd = (oxd -. px) *. dcos (-p.pa) -. (oyd -. py) *. dsin (-p.pa) in
      let yd = (oyd -. py) *. dcos (-p.pa) +. (oxd -. px) *. dsin (-p.pa) in
      rs := new_segmentSimpleFloat xo yo xd yd

let clipSegment rs p =
  let (xo,yo),(xd,yd) = Segment.real_coord !rs in 
  if  xo < 1. && xd < 1. then raise NePasTraiter
  else if xo < 1. then rs := new_segmentSimpleFloat 1. 
          (yo +. (1. -. xo) *. (tangle !rs))
          xd yd
  else if xd < 1. then rs := new_segmentSimpleFloat xo yo 1. 
            (yd +. (1. -. xd) *. (tangle !rs));
            if xo > max_dist then rs := new_segmentSimpleFloat max_dist yo xd yd ;
          if xd > max_dist then rs:= new_segmentSimpleFloat xo yo max_dist yd

(*Projette le segment sur l'écran et renvoie les sommets du polygone*)
let projectionSegment rs =
  let d_focale = (float_of_int win_w /. 2.) /. (dtan (fov / 2)) in
  let (xo,yo),(xd,yd) = Segment.real_coord !rs in
  let win_w = float_of_int win_w in
  let nyo,nyd = (win_w /. 2.)  -. ((yo *. d_focale) /. xo), (win_w /. 2.)  -. ((yd *. d_focale) /. xd)  in
  if nyo < 0. && nyd < 0. || nyo > win_w && nyd > win_w then raise NePasTraiter
  else let win_h = float_of_int win_h in 
  let hsDiv = win_h /. 2. in
  let zc x = hsDiv +. (float_of_int (ceiling_h - wall_h) *. d_focale) /. x in
  let zf x = hsDiv +. (float_of_int (floor_h - wall_h) *. d_focale) /. x in
  let zco, zfo, zcd, zfd = zc xo, zf xo, zc xd, zf xd in
  truncate nyo, truncate zco, truncate zfo, truncate nyd, truncate zcd, truncate zfd
  
           
let parseFunction3d p contour fill s = 
    let segment = ref s in
    try
      let () = rotateSegment segment p ; clipSegment segment p in
      let nyo, zco, zfo, nyd, zcd, zfd = projectionSegment segment in
      if fill then
      set_color Options.fill_color ;
      fill_poly [|nyo,zco; nyo, zfo ; nyd, zfd; nyd, zcd|];
      revert_color () ;
      if contour then
      set_color Options.contour_color ;
      draw_segments [|nyo, zco, nyo, zfo|];
      draw_segments [|nyo, zfo, nyd, zfd|];
      draw_segments [|nyo, zco, nyd, zcd|];
      draw_segments [|nyd, zfd, nyd, zcd|];
      revert_color () 
    with NePasTraiter -> ()




let display bsp p =
  let parseFunction2d = drawSegment in
  let parseMiniMap = drawSegmentScale Options.scale in
  let not_zero x = if x <= 1 then 1 else x in
        match mode with
        | TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y size2d ; set_color black
        | ThreeD ->  clear_graph () ; fill_background Options.bg ; set_color coral ; 
                    (*fill_rect 0 (win_h/2) win_w (win_h/2) ;*) revert_color (); 
                    Bsp.rev_parse (parseFunction3d p !Options.draw_contour !Options.fill_wall) bsp (p.pos) ;
                    if Options.minimap then begin
                    set_color white ; Bsp.rev_parse parseMiniMap bsp (p.pos); revert_color () ; set_color red ;
                    fill_circle (p.pos.x / scale) (p.pos.y/scale) (not_zero (size2d/scale)) ; 
                    drawSegment p.lAngleMinimap ; drawSegment p.rAngleMinimap;
                    revert_color ()
                    end
                    

