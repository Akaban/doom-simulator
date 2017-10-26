(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Bryce Tichit *)
open Point
open Segment
open Graphics
open Printf
open Player
open Bsp
open Options
open Colors

exception NotAnAction

let drawSegment s =
  let (xo,yo),(xd,yd) = real_coord s in
  Graphics.draw_segments [|(truncate xo,truncate yo,truncate xd,truncate yd)|]

let not_zero x = if x=0 then 1 else x
let rec draw2D bsp p scale = if scale=0 then ()
                     else begin clear_graph () ; Bsp.iter (Render.drawSegmentScale scale) bsp ;
                     set_color red; begin let sp = divPoint p scale in 
                     fill_circle sp.x sp.y (not_zero (size2d)) end ; 
                     revert_color () ; synchronize ();  match (read_key ()) with 'i' -> draw2D bsp p (scale+1) | _ -> clear_graph ()
                     end

let drawCollisionZone s =
  if s.segBottom = None then () 
  else let getSome = fromSome s in
  let segs = [getSome s.segBottom; getSome s.segLeft; getSome s.segRight; getSome s.segTop]
  in List.iter drawSegment segs

let printSegId s =
  if s.segBottom = None then ()
  else let p = bottomRight s in
  let decal = if s.porig.x >= s.pdest.x then -10 else 10 in
  let cx, cy = current_x (), current_y () in
  moveto (p.x+decal) p.y ;
  draw_string (string_of_int s.id);
  moveto cx cy

let getHead = function
  | E -> failwith "tkt"
  | N(s,_,_) -> s

let followSeg = ref None

let followSegPrint = function
  | None -> "Vous ne suivez aucun segment"
  | Some s -> "Vous suivez " ^ toString s

let followSegBool = function
  | None -> false
  | Some _ -> true

(* Prends une paire d'option et renvoie le premier element qui n'est pas None sinon None*)
let firstSome = function
  | Some s,_ 
  | _, Some s -> Some s
  | None,None -> None

let rec segId id = function
  | E -> None
  | N(s,lt,rt) -> if s.id=id then Some s 
                  else firstSome (segId id lt,segId id rt)

let hardFromSome = function
  | Some s -> s
  | _ -> failwith "hardFromSome with None" 

let pauseGame pauseKey =
  printf "Jeu en pause, appuyer sur %c pour reprendre la partie\n" pauseKey ;
  flush stdout;
  let key = ref None in
  while !key <> Some pauseKey do
    let ev = wait_next_event [Key_pressed] in
    if ev.keypressed then key := Some ev.key;
  done ; ()


let debugKeys3D k player bsp runData =
  match k with
    | 'c' -> printf "Clear graph\n" ; flush stdout;
             clear_graph()
    | 'n' -> printf "Afficher contour\n"; flush stdout;
             Options.draw_contour := (not !Options.draw_contour)
    | 'f' -> printf "Afficher murs\n"; flush stdout;
             Options.fill_wall := (not !Options.fill_wall)
    | 'o' -> Options.collision := not !Options.collision ; if !Options.collision then printf "Collision enabled\n" else printf "collision disabled\n" ; flush stdout 
    | 'm' -> Options.debug := not !Options.debug ; if !Options.debug then printf "Option debug activated\n" else printf "Option debug disabled\n" ; flush stdout
    | 'p' -> pauseGame 'p'
    | 'i' -> runData.playerInfo <- not runData.playerInfo ; if runData.playerInfo then printf "Option %s activated\n" "info-joueur"
            else printf "Option %s disabled\n" "info-joueur" ; flush stdout
    | _ -> raise NotAnAction

let debugKeys2D k player bsp = 
    match k with
      | 'j' -> printf "Affichage des  zones de collisions des segments\n"; flush stdout;
               set_color red ; Bsp.iter drawCollisionZone bsp ; set_color black
      | 'v' -> printf "Affiche des id de segment\n" ; flush stdout; set_text_size 20;
               set_color blue ; Bsp.iter printSegId bsp ; set_color black 
      | 'g' -> printf "Affichage de tout les segments dispo\n";flush stdout ;
               Bsp.iter (fun s -> printf "%s\n" (toString s) ; flush stdout) bsp
      | 'p' -> printf "Les coordonnées du joueur sont %s avec un angle de %d\n" (Point.toString player.pos) player.pa ; flush stdout
      | 'm' -> Options.debug := not !Options.debug ; if !Options.debug then printf "Option debug activated\n" else printf "Option debug disabled\n" ; flush stdout
      | ',' -> if not (followSegBool !followSeg) then begin printf "Je regrette mais vous ne suivez aucun segment, pour changer de segment k\n" ; flush stdout end
               else let seg = hardFromSome !followSeg in let pos = get_position player.pos seg in 
                    printf "Votre position par rapport a ce segment est %s\n" (tposToString pos) ; flush stdout
      | 'f' -> printf "Suivi d'un segment: %s " (followSegPrint !followSeg); flush stdout ; printf "pour changer le segment: k\n" ; flush stdout
      | 'k' -> printf "Entrer l'id du nouveau segment a suivre\n" ; flush stdout ;
               let idS = read_line () in
               let () = printf "Le programme a recup %s\n" idS in
               let id = int_of_string idS in 
               let seg = segId id bsp in begin match seg with
                                                | Some s -> printf "J'ai trouve le segment d'id %d, vous suivez desormais ce segment\n" id;
                                                            followSeg := Some s
                                                | None -> printf "Je regrette mais je n'ai pas trouve ce segment (%d)\n" id
                                         end ; flush stdout
      | 'o' -> Options.collision := not !Options.collision ; if !Options.collision then printf "Collision enabled\n" else printf "collision disabled\n" ; flush stdout 
      | 'x' -> printf "Affichage de tout les segments a gauche du pivot\n"; flush stdout ;
               printf "Par rapport au pivot %s \n" (toString (getHead bsp)) ; flush stdout ;
               Bsp.iter (fun s -> printf "%s\n" (toString s); flush stdout) (Bsp.getLeft bsp)
      | 'w' -> printf "Affichage de tout les segments a droite du pivot\n"; flush stdout ;
               printf "Par rapport au pivot %s \n" (toString (getHead bsp)) ; flush stdout ;
               Bsp.iter (fun s -> printf "%s\n" (toString s); flush stdout) (Bsp.getRight bsp)
      | _ -> raise NotAnAction ;;
                

let debugKeys k player bsp runData = match Options.mode with
                             | Options.TwoD -> debugKeys2D k player bsp
                             | Options.ThreeD -> debugKeys3D k player bsp runData
