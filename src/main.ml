open Options
open Graphics
open Player
open Point

let keyToDir = function
  | 'z' -> Some MFwd
  | 'q' -> Some MLeft
  | 's' -> Some MBwd
  | 'd' -> Some MRight
  | _ -> None

let () =
    let (px,py,pa),seglist = Parse_lab.read_lab cin
    in let seglist2 = List.map (fun (xo,yo,xd,yd) -> Segment.new_segment xo yo xd yd) seglist
    in let bsp = Bsp.build_bsp seglist2
    in let player = Player.new_player (Point.new_point px py) pa
    in open_graph (Printf.sprintf " %dx%d" win_w win_h);
       auto_synchronize false; Render.display bsp player ; synchronize () ;
       try
           while true do
               Render.display bsp player;
               let ev=wait_next_event [Key_pressed] in
               if ev.keypressed then begin
                 match keyToDir ev.key with
                  | Some m -> 
                     
                      move m player bsp
                  | _ -> () end;
               synchronize ();
           done;
       with Exit -> close_graph () ; exit 0;;
