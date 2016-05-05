open Point
open Segment
open Graphics
open Printf
open Player
open Bsp

let drawSegment s =
  let (xo,yo),(xd,yd) = real_coord s in
  Graphics.draw_segments [|(truncate xo,truncate yo,truncate xd,truncate yd)|]

let drawCollisionZone s =
  if s.segBottom = None then () 
  else let getSome = fromSome s in
  let segs = [getSome s.segBottom; getSome s.segLeft; getSome s.segRight; getSome s.segTop]
  in List.iter drawSegment segs

let getHead = function
  | E -> failwith "tkt"
  | N(s,_,_) -> s

let debugKeys k player bsp =
    match k with
      | 'c' -> printf "Affichage des  zones de collisions des segments\n"; flush stdout;
               set_color red ; Bsp.iter drawCollisionZone bsp ; set_color black
      | 'g' -> printf "Affichage de tout les segments dispo\n";flush stdout ;
               Bsp.iter (fun s -> printf "%s\n" (toString s) ; flush stdout) bsp
      | 'r' -> printf "Reset map\n" ; flush stdout ; clear_graph () ; Render.display bsp player ; synchronize ()
      | 'p' -> printf "Les coordonnées du joueur sont %s avec un angle de %d\n" (Point.toString player.pos) player.pa ; flush stdout
      | 'm' -> Options.debug := not !Options.debug ; if !Options.debug then printf "Option debug activated" else printf "Option debug disabled" ; flush stdout
      | 'b' -> let segment = getHead bsp in printf ""; flush stdout;
               let pos = get_position player.pos segment in printf "Votre position par rapport a ce segment est %s\n" (tposToString pos) ; flush stdout
                                             
      | _ -> () 
                
