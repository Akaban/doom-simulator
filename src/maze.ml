(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Bryce Tichit *)
open Printf
open Graphics

type t = {
  rank : int array;
  parent : int array;
}

type dir = N | S | E | O
type wallD = { i : int ; j : int ; pos : dir ; mutable state : bool }
type wall = wallD ref
type cell = {
  i : int;
  j : int;
  wallN : wall;
  wallO : wall;
  wallE : wall;
  wallS : wall}

let new_cell i j wn wo we ws = {i=i;j=j;wallN=wn;
                    wallO=wo;wallE=we;wallS=ws}
let new_cell i j wn wo we ws = Some (new_cell i j wn wo we ws)
let new_wall i j d = {i=i;j=j;pos=d;state=true}
let new_wall i j d = ref (new_wall i j d)


let getCell = function Some c -> c | None -> raise (Invalid_argument "getCell with none") 

(*on construit le labyrinthe avec tout les murs fermé et on 
 * définit bien les murs partagés par plusieurs cellules avec 
 * les références*)
let makeEdges taille =
  let edges = Array.make_matrix taille taille None in
  let walls = ref [] in
  let addW w = walls := w::!walls ; w in (*on doit tenir a jour la liste des murs dispo*)
  for j=0 to taille-1 do
    for i=0 to taille-1 do
      edges.(i).(j) <- begin
        new_cell i j 
        (if j=0 then (new_wall i j N) else (getCell (edges.(i).(j-1))).wallS) 
        (if i=0 then (new_wall i j O) else (getCell (edges.(i-1).(j))).wallE)
        (if i=taille-1 then new_wall i j E else let w = new_wall i j E in addW w)
        (if j=taille-1 then new_wall i j S else let w = new_wall i j S in addW w)
      end;
    done;
  done;edges,!walls;;

(*creation de la structure union find*)
let create n =
  { rank = Array.create n 0;
    parent = Array.init n (fun i -> i) }

let rec find t i =
  let p = t.parent.(i) in
  if p = i then
    i
  else begin
    let r = find t p in
    t.parent.(i) <- r;
    r
  end

let union t i j =
  let ri = find t i in
  let rj = find t j in
  if ri <> rj then begin
    if t.rank.(ri) < t.rank.(rj) then
      t.parent.(ri) <- rj
    else begin
      t.parent.(rj) <- ri;
      if t.rank.(ri) = t.rank.(rj) then 
        t.rank.(ri) <- t.rank.(ri) + 1
    end
  end

let () = Random.self_init ()
(*Renvoie un labyrinthe parfait sous forme de matrice de Cell option*)
let makeMaze taille =
  let edges,walls = makeEdges taille in
  let shuffle = List.sort (fun _ _ -> (fun b -> if b then 1 else -1) (Random.bool ())) in
  let walls = shuffle walls in
  let getNextCell i j = function N -> i,(j-1) | S -> i,(j+1) | O -> (i-1),j |E -> (i+1),j in
  let u = create (taille*taille) in
  let cpt = ref 0 in
  let rec openWall (ws : wall list) = match ws with
    | w::ws ->
          let (nexti,nextj) = getNextCell !w.i !w.j !w.pos in
          let k, k' = !w.i + !w.j * taille, nexti + nextj * taille in
          if find u k <> find u k' then begin
            !w.state <- false; cpt := !cpt + 1 ; (*on ouvre un mur si les classes d'equivalence*)
            union u k k'; openWall ws end         (* entre les 2 cell adjacentes sont différentes*)
          else w :: openWall ws
    | [] -> []
  in let maxb = taille * taille - 1 in (*on a un labyrinthe parfait ssi on a ouvert mn-1 murs*)
  let wallsRef = ref walls in
  while !cpt < maxb do
    let remainingWalls = openWall !wallsRef in
    wallsRef := shuffle remainingWalls 
  done ; (*choix d'un mur aléatoire entree et sortie*)
  let i,j = Random.int (taille-1), Random.int (taille-1) in (*ouverture aléatoire des murs
                                                            *d'entrée et de sortie*)
  !((getCell (edges.(i).(taille-1))).wallS).state <- false ;
  !((getCell (edges.(j).(0))).wallN).state <- false; (i,taille-1),(j,0), edges;;

(*renvoie l'ensemble des murs sous forme de liste de segment*)
let maze_of_edges e taille void decal =
  let l = ref [] in
  for j=0 to taille-1 do
    for i=0 to taille-1 do
      begin
      let cell= getCell e.(i).(j) in
      let j,halfW = taille - 1 - j,0 in 
(*gauche*)   if !(cell.wallO).state then l := (i * void + decal , j * void + halfW + decal,i * void + decal,(j+1) * void + decal) :: !l ;
(*haut*)     if !(cell.wallN).state then l := (i * void + decal,  (j+1) * void + decal, (i+1) * void + decal,  (j+1) * void+decal) :: !l;
(*droite*)   if !(cell.wallE).state then l := ((i+1)*void+ decal, j * void + (halfW)+ decal,  (i+1)*void+ decal, (j+1) * void - (halfW) + decal) :: !l ;
(*bas*)      if j=0 && !(cell.wallS).state then l := (i* void + decal, j * void+ decal,  (i+1)*void + decal,  j * void+ decal) :: !l ;
      end ;
    done
  done;!l 

(* convertit les segments en Segment.t *)
let getMazeSegments taille width decal =
  let (px,py),(pxe,pye),maze = makeMaze taille in
  List.map (fun (a,b,c,d) -> Segment.new_segment a b c d) (maze_of_edges maze taille width decal),
  (px*width+1+decal,(taille-2-py)*width+decal),(pxe*width+decal,((taille-pye)*width +decal))






