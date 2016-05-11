type t = {x : int; y : int}

let new_point x y = {x=x;y=y}

let mapPoint f p = f p.x, f p.y

let distance a b = 
  let (xo,yo),(xd,yd) = mapPoint float_of_int a,mapPoint float_of_int b
  in sqrt ((xd-.xo) ** 2. +. (yd-.yo) ** 2.)

let divPoint p scale = new_point (p.x / scale) (p.y / scale)

let toString p = "(x=" ^ string_of_int p.x ^ ",y=" ^ string_of_int p.y ^ ")"

let translateVect (dx,dy) alpha = new_point (truncate (dx *. Trigo.dcos alpha -. dy *. Trigo.dsin alpha)) 
                                               (truncate ( dx *. Trigo.dsin alpha +. dy *. Trigo.dcos alpha))
let translateVectFloat (dx,dy) alpha = dx *. Trigo.dcos alpha -. dy *. Trigo.dsin alpha ,
                                       dx *. Trigo.dsin alpha +. dy *. Trigo.dcos alpha
let translatePointFloatWithAngle (x,y) (dx,dy) alpha =
  let (tVectx,tVecty) = translateVectFloat (dx,dy) alpha in
  x +. tVectx, y +. tVecty


let translatePoint p vectPoint = new_point (p.x + vectPoint.x) (p.y + vectPoint.y)

let translatePointWithAngle p (dx,dy) alpha =
  let tVect = translateVect (dx,dy) alpha in
  translatePoint p tVect


