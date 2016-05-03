open Options
open Graphics

let () =
    let (px,py,pa),seglist = Parse_lab.read_lab cin
    in let seglist2 = List.map (fun (xo,yo,xd,yd) -> Segment.new_segment xo yo xd yd) seglist
    in let bsp = Bsp.build_bsp seglist2
    in let player = Player.new_player (Point.new_point px py) pa
    in open_graph (Printf.sprintf " %dx%d" win_w win_h);
       auto_synchronize false;
       try
           while true do
               wait_next_event [Button_down];
               Render.display bsp player;
               synchronize ();
           done
       with Exit -> close_graph () ; exit 0;;
