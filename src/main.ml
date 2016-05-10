open Options
open Graphics
open Player
open Point
open Colors
open Debug

let keyToDir = function
  | 'z' -> Some MFwd
  | 'q' -> Some MLeft
  | 's' -> Some MBwd
  | 'd' -> Some MRight
  | _ -> None


let actions k player bsp = match k with
  | 'e' -> rotate Right player
  | 'a' -> rotate Left player
  | _ -> Debug.debugKeys k player bsp

let mouseDirection (x1,y1) (x2,y2) =
  let mouseSegment = Segment.new_segmentSimple x1 y1 x2 y2 in
  let (ovSx,_) = Segment.originVector mouseSegment
  in None (*
  in if ovSx >= mouse_sensitivity then Some Right else
     if ovSx <= -mouse_sensitivity then Some Left
     else None*)

let () =
  let (px,py,pa),seglist = Parse_lab.read_lab cin in
  let seglist2 = List.map (fun (xo,yo,xd,yd) -> Segment.new_segment xo yo xd yd) seglist in
  let bsp = Bsp.build_bsp seglist2 in
  let player = Player.new_player (Point.new_point px py) pa in
  Bsp.instanceBsp := bsp ;
  open_graph (Printf.sprintf " %dx%d" win_w win_h); set_window_title "Doom-Like Project 0.1";
       auto_synchronize false; Render.display bsp player ; synchronize () ;
       try
         while true do
          try
           let mx, my = mouse_pos () in
           let ev=wait_next_event [Key_pressed;Mouse_motion] in
           begin
           if ev.keypressed then
             match keyToDir ev.key with
                  | Some m -> move m player !Bsp.instanceBsp 
                  | _ -> actions ev.key player !Bsp.instanceBsp (*Debug.debugKeys ev.key player bsp*) 
          else let dirAngle = mouseDirection (mx, my) ((ev.mouse_x), (ev.mouse_y)) in
            match dirAngle with
                | Some Right -> rotate Right player
                | Some Left  -> rotate Left player 
                | None -> raise NotAnAction end;
               Render.display !Bsp.instanceBsp player;
               synchronize ();
          with Debug.NotAnAction -> ();
         done;
  with Exit -> close_graph () ; exit 0;;
