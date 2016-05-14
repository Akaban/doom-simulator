open Printf
open Options
open Graphics
open Player
open Point
open Colors
open Debug

exception NoFileOpened

let newRunData initPosX initPosY initAngle =
  { labInitPos=new_point initPosX initPosY; labInitAngle=initAngle;
   playerInfo=false}

let keyToDir = function
  | 'z' -> Some MFwd
  | 'q' -> Some MLeft
  | 's' -> Some MBwd
  | 'd' -> Some MRight
  | _ -> None


let actions k player bsp runData = match k with
  | 'e' -> rotate 10 Right player
  | 'a' -> rotate 10 Left player
  | 'c' -> crouchPlayer player
  | 'b' -> rushPlayer player
  | 'u' -> Debug.draw2D bsp scale
  | ' ' -> raise NotAnAction (*Render.jumpAnimation bsp player runData*)
  | 'r' -> tp (runData.labInitPos.x,runData.labInitPos.y,runData.labInitAngle) player bsp
  | '\027' (*echap*) -> raise Exit
  | _ -> Debug.debugKeys k player bsp runData

let mouseDirection (x1,y1) (x2,y2) =
  let mouseSegment = Segment.new_segmentSimple x1 y1 x2 y2 in
  let (ovSx,_) = Segment.originVector mouseSegment
  in if ovSx >= mouse_sensitivity then Some Right else
     if ovSx <= -mouse_sensitivity then Some Left
     else None

let readLab () = match maze with
  | true -> let m = Maze.getMazeSegments maze_size maze_intensity (win_h/10) in 300,40,90,m   
  | _ -> match cin with None -> raise NoFileOpened | Some cin ->
          let (px,py,pa),seglist = Parse_lab.read_lab cin in
          let seglist2 = List.map (fun (xo,yo,xd,yd) -> Segment.new_segment xo yo xd yd) seglist in
          px, py, pa, seglist2

let () =
  let px,py,pa,seglist = readLab () in
  let bsp = Bsp.build_bsp seglist in
  let player = Player.new_player (Point.new_point px py) pa in
  let runningData = newRunData px py pa in
  Bsp.instanceBsp := bsp ;flush stdout;
  let s = Printf.sprintf " %dx%d" win_w win_h in
  open_graph s; set_window_title "Doom-Like Project 0.1";
       auto_synchronize false; Render.display bsp player runningData ; synchronize () ;
       try
         while true do
          try
           let mx, my = mouse_pos () in
           let ev=wait_next_event [Key_pressed;Mouse_motion] in
           begin
           if ev.keypressed then
             match keyToDir ev.key with
                  | Some m -> move m player !Bsp.instanceBsp 
                  | _ -> printf "Key typed: %c\n" ev.key ;flush stdout; actions ev.key player !Bsp.instanceBsp runningData
          else if ev.button then ()
          else let dirAngle = mouseDirection (mx, my) ((ev.mouse_x), (ev.mouse_y)) in
            match dirAngle with
                | Some Right -> rotate Options.angular_change Right player
                | Some Left  -> rotate Options.angular_change Left player 
                | None -> raise NotAnAction end;
               Render.display !Bsp.instanceBsp player runningData;
               synchronize ();
          with Debug.NotAnAction -> ();
         done;
  with Exit -> close_graph () ; exit 0;;
