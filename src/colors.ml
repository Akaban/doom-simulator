(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Abdelkader-Mahamat Djamal & Bryce Tichit *)
(* Module Colors permettant d'étendre la partie
 * Colors du module Graphics*)
open Graphics

let grey = rgb 160 160 160
let coral = rgb 83 37 28
let bluesky = rgb 178 229 239
let orange = rgb 255 92 0
let indigo = rgb 75 0 130
let purple = rgb 128 0 128

let current = ref black
let previous = ref None

let set_color c = set_color c ; previous := Some !current ; current := c
let revert_color () = match !previous with
  | Some c -> set_color c ; previous := None
  | None -> ()


let rec color_of_int = function
  |0 -> red
  |1 -> orange
  |2 -> yellow
  |3 -> green
  |4 -> blue
  |5 -> indigo
  |6 -> purple
  |x -> color_of_int (x mod 7)
