open Graphics
open Segment
open Trigo
open Player
open Point
open Options
open Colors
open Printf

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

let compteur =
  let x = ref 0 in fun () -> x := !x +1 ; !x;; 

let rotateSegment rs p =
  let (oxo,oyo),(oxd,oyd) = Segment.real_coord !rs in
  let px, py = float_of_int p.pos.x, float_of_int p.pos.y in
      let xo = (oxo -. px) *. dcos (-p.pa) -. (oyo -. py) *. dsin (-p.pa) in
      let yo = (oyo -. py) *. dcos (-p.pa) +. (oxo -. px) *. dsin (-p.pa) in
      let xd = (oxd -. px) *. dcos (-p.pa) -. (oyd -. py) *. dsin (-p.pa) in
      let yd = (oyd -. py) *. dcos (-p.pa) +. (oxd -. px) *. dsin (-p.pa) in
      let pox, poy = translateVectFloat (oxo-.px,oyo-.py) (-p.pa) in
      let pdx, pdy =  translateVectFloat (oxd-.px,oyd-.py) (-p.pa) in
      rs := new_segmentSimpleFloatWithid pox poy pdx pdy !rs.id

let clipSegment rs p =
  let (xo,yo),(xd,yd) = Segment.real_coord !rs in 
  if  xo < 1. && xd < 1. then raise NePasTraiter
  else if xo < 1. then rs := new_segmentSimpleFloatWithid 1. 
          (yo +. (1. -. xo) *. (tangle !rs))
          xd yd !rs.id
  else if xd < 1. then rs := new_segmentSimpleFloatWithid xo yo 1. 
            (yd +. (1. -. xd) *. (tangle !rs)) !rs.id;
            if xo > max_dist then rs := new_segmentSimpleFloatWithid max_dist yo xd yd !rs.id ;
          if xd > max_dist then rs:= new_segmentSimpleFloatWithid xo yo max_dist yd !rs.id

(*Projette le segment sur l'écran et renvoie les sommets du polygone*)
let projectionSegment rs =
  let d_focale = (float_of_int win_w /. 2.) /. (dtan (fov / 2)) in
  let (xo,yo),(xd,yd) = Segment.real_coord !rs in
  let win_w = float_of_int win_w in
  let nyo,nyd = (win_w /. 2.)  -. ((yo *. d_focale) /. xo), (win_w /. 2.)  -. ((yd *. d_focale) /. xd)  in
  if nyo < 0. && nyd < 0. || nyo > win_w && nyd > win_w then raise NePasTraiter
  else
  let win_h = float_of_int win_h in 
  let hsDiv = win_h /. 2. in
  let zcConst, zfConst = hsDiv +. (float_of_int (ceiling_h - eye_h) *. d_focale) ,
                         hsDiv +. (float_of_int (floor_h - eye_h) *. d_focale)  in
  let zc x = hsDiv +. (float_of_int (ceiling_h - eye_h) *. d_focale) /. x in
  let zf x = hsDiv +. (float_of_int (floor_h - eye_h) *. d_focale) /. x in
  let zco, zfo, zcd, zfd = zc xo, zf xo, zc xd, zf xd in
  let du, dl = (zcd -. zco) /. (nyd -. nyo), (zfd -. zfo) /. (nyd -. nyo) in
  let nyo, zco, zfo = if nyo < 0. then 0., zco -. (nyo *. du), zfo -. (nyo *. dl)
                      else if nyo > win_h then win_h, zco -. ((nyo -. win_h) *. du), zfo -. ((nyo -. win_h) *. dl)
                      else nyo,zco,zfo in
  let nyd, zcd, zfd = if nyd < 0. then 0., zcd -. (nyd *. du), zfd -. (nyd *. dl)
                      else if nyd > win_h then win_h, zcd -. ((nyd -. win_h) *. du), zfd -. ((nyd -. win_h) *. dl)
                      else nyd,zcd,zfd in
  let nyo, zco, zfo, nyd, zcd, zfd = truncate nyo, truncate zco, truncate zfo, truncate nyd, truncate zcd, truncate zfd in
  if !Options.debug then begin
    Printf.printf "Segment nb %d, porig: (%d,%d) porigUp: (%d,%d) pdest: (%d,%d) pdestUp :(%d,%d)\n" !rs.id 
                nyo zco nyo zfo nyd zfd nyd zcd; flush stdout; 
    Printf.printf "             which has rotated coordinates porig=(%d,%d) pdest=(%d,%d)\n" 
    (truncate xo) (truncate yo) (truncate xd) (truncate yd) ; flush stdout ;
  end ;
  nyo, zco, zfo, nyd, zcd, zfd
  
           
let parseFunction3d p contour fill s = 
    let segment = ref s in
    try
      let () = rotateSegment segment p ; clipSegment segment p in
      let nyo, zco, zfo, nyd, zcd, zfd = projectionSegment segment in
      if fill then begin
      set_color Options.fill_color ;
      fill_poly [|nyo,zco; nyo, zfo ; nyd, zfd; nyd, zcd|];
      revert_color () end ;
      if contour then begin
      set_color Options.contour_color ;
      draw_segments [|nyo, zco, nyo, zfo|];
      draw_segments [|nyo, zfo, nyd, zfd|];
      draw_segments [|nyo, zco, nyd, zcd|];
      draw_segments [|nyd, zfd, nyd, zcd|];
      revert_color () end 
    with NePasTraiter -> ()




let display bsp p =
  let parseFunction2d = drawSegment in
  let parseFunction2ddebug s =
    let rs = ref s in
    let () = rotateSegment rs p in
    let (xo,yo),(xd,yd) = Segment.real_coordInt !rs in
    let decX,decY = win_w / 2, win_h/2 in
    draw_segments [|xo + decX, yo + decY, xd + decX , yd + decY|] in
  let parseMiniMap = drawSegmentScale Options.scale in
  let not_zero x = if x <= 1 then 1 else x in
        match mode with
        | TwoD -> Bsp.parse parseFunction2d bsp (p.pos) ; set_color white ; fill_circle p.oldpos.x p.oldpos.y size2d ;
          set_color blue ; fill_circle p.pos.x p.pos.y size2d ; set_color black
        | ThreeD ->  clear_graph () ; fill_background Options.bg ; set_color coral ; 
                    (*fill_rect 0 (win_h/2) win_w (win_h/2) ;*) revert_color (); 
                    Bsp.rev_parse (parseFunction3d p !Options.draw_contour !Options.fill_wall) bsp (p.pos) ;
                    if !Options.debug then begin
                      Printf.printf "\n_____________________________________________________________\n" ; flush stdout end ;
                    if Options.minimap then begin
                    set_color white ; Bsp.rev_parse parseMiniMap bsp (p.pos); revert_color () ; set_color red ;
                    fill_circle (p.pos.x / scale) (p.pos.y/scale) (not_zero (size2d/scale)) ; 
                    drawSegment p.lAngleMinimap ; drawSegment p.rAngleMinimap;
                    revert_color ()
                    end
        | TwoDdebug -> clear_graph() ; Bsp.parse parseFunction2ddebug bsp (p.pos) ;
                       let ls,rs = Player.calculateAngleMinimapS (new_point (win_w/2) (win_h/2)) 0 false in
                       set_color red ;
                       drawSegment ls ; drawSegment rs ; revert_color ()
                    

