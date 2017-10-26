(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Bryce Tichit *)
let pi = 4. *. atan 1.

let pidiv = pi /. 180.
let ipidiv = 180. /. pi

let d_to_rad a = float a *. pidiv
let r_to_deg a = a *. ipidiv

let rtan a = tan a
let dtan a = tan (d_to_rad a)

let dcos a = cos (d_to_rad a)

let dacos c = r_to_deg (acos c)

let datan c = r_to_deg (atan c)

let dsin a = sin (d_to_rad a)

