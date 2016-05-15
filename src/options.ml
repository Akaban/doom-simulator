(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Abdelkader-Mahamat Djamal & Bryce Tichit *)
type tmode = TwoD | ThreeD

(*des informations utiles sur la partie que l'on
 * pourra donner à actions *)
type runningData =
  { labInitPos : Point.t ;
    labInitAngle : int ;
    mutable playerInfo : bool;
    mazeEndpos : Point.t
  }

let usage = "usage: ./bsp file.lab"
let file = ref ""

let mode = ref TwoD

let size2d = ref 4

let win_w = ref 800
let win_h = ref 800

let mouse_sensitivity = ref 1
let angularChange = ref 1

let max_dist = ref 20000

let fov = ref 60

let step_dist = ref 10

let xmin = ref 1
let xmax = ref 5000

let scale = ref 5
let minimap = ref false

let maze = ref false
let maze_size = ref 10
let maze_intensity = ref 200

let debug = ref false
let debug_bsp = ref false

let minimap_xmax = ref 400

let rainbow = ref false

let set_mode = function
  | "2D" -> mode := TwoD
  | "3D" -> mode := ThreeD
  | _ -> raise (Arg.Bad "2D or 3D only")


let specs = 
  [ "-mode", Arg.String set_mode, "<2D | 3D> 2D or 3D display";
    "-fov", Arg.Set_int fov, " field of vision (angle de vision)";
    "-dims", Arg.Tuple [Arg.Set_int win_w; Arg.Set_int win_h], 
    " set the dimensions of the graph";
    "-scale", Arg.Set_int scale, " scale of the 2D map";
    "-map", Arg.Set minimap, " set a minimap in the lower left corner";
    "-step", Arg.Set_int step_dist, " set the distance between two steps";
    "-xmin", Arg.Set_int xmin, " set minimum distance of display";
    "-debug", Arg.Set debug, " debugging 2D rendering";
    "-debugbsp", Arg.Set debug_bsp, " debugging bsp";
    "-maze", Arg.Set maze, " generate random maze";
    "-mazesize", Arg.Set_int maze_size, " set size of maze";
    "-mazewidth", Arg.Set_int maze_intensity, " set width of maze's walls";
    "-xmax", Arg.Set_int xmax, " set maximum distance of display";
    "-rainbow", Arg.Set rainbow, " set rainbow colors to walls";
    "-minimap_xmax", Arg.Set_int minimap_xmax, " set maximum distance of display in minimap";
   ]

let alspecs = Arg.align specs

let set_file ofile labext s =
    if Filename.check_suffix s ".lab" then ofile := Some s
    else labext := false

let cin = 
  let ofile, labext = ref None, ref true in 
  Arg.parse alspecs (set_file ofile labext) usage;
  if !maze then None else
  match !ofile with 
    | Some f -> if !labext then (file := f ; Some (open_in f)) else raise (Arg.Bad "No .lab extension")
    | None -> raise (Arg.Bad "no file provided")


let maze = !maze
let maze_size = !maze_size
let maze_width = !maze_intensity


let rainbow = !rainbow

let file = !file

let win_w = !win_w
let win_h = !win_h

let mouse_sensitivity = !mouse_sensitivity
let angular_change = !angularChange

let max_dist = float !max_dist

let xmax = float !xmax

let xmin = float !xmin

let ceiling_h = win_h / 3
let floor_h = 0
let wall_h = ceiling_h - floor_h
let eye_h_debout = wall_h - wall_h/2
let eye_h_accroupi = eye_h_debout / 2
let eye_h = ref eye_h_debout

let mode = !mode

let size2d = !size2d

let wc_size = float !step_dist *. 1.5
let wc_maze_size = float !step_dist /. 8.
let wall_collision_size = ref wc_size

let angleMiniMap = 23
let sizeAngleMiniMap = float !step_dist

let fov = !fov

(* draw options *)
let defaultCeilingh = truncate (float win_h /. 2.) (*hauteur de début du plafond (affichage)*)
let ceilingh = ref defaultCeilingh
let ceilingMultiplicatorRange = 1500
let ceilingMultiplicator = 1. /. 300.
let ceilingMultiplicator2 = 1. /. 1000.

let bg = Colors.grey
let minimap_color = Graphics.black
let ceiling_color = Colors.bluesky
let fill_color = Graphics.white
let contour_color = Graphics.black
let draw_contour = ref true
let fill_wall = ref true

let collision = ref true

(*si notre joueur est accroupi alors il est logique qu'il
 * avance moins vite donc un step_dist moindre *)
let step_dist_debout = float !step_dist
let step_dist_accroupi = step_dist_debout /. 2.
let step_dist_rush = step_dist_debout *. 2.
let step_dist = ref step_dist_debout

(*gravite/saut*)
let gravity = 20.
let jumpPeak = 15
let jumpSpeed = 7. (*chaque seconde prends jumpSpeed de hauteur jusqu'a jumpPeak*)

let scale = !scale
let minimap = !minimap
let minimap_xmax = !minimap_xmax

let debug_manualRender = ref false

let debug = ref false
let debug_bsp = !debug_bsp


