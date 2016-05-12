open Segment
open List

type tint = F | N of int * tint * tint
type t = E | N of Segment.t * t * t 

let rec bspId = function
  | E -> F
  | N(s,l,r) -> N(s.id,bspId l,bspId r)

let getRight = function
  | N(_,_,rt) -> rt
  | E -> E

let getLeft = function
  | N(_,lt,_) -> lt
  | E -> E

let rec parse f bsp p = match bsp with
  | N(s,lt,rt) -> 
      begin
       match (Segment.get_position p s) with
         | L -> parse f lt p ; f s ; parse f rt p
         | _ -> parse f rt p ; f s ; parse f lt p
      end
  | E -> ()

let rec toList = function
  | N(s,lt,rt) -> rev_append (s :: toList lt) (toList rt)
  | E -> []


let rec rev_parse f bsp p = match bsp with
  | N(s,lt,rt) -> 
      begin
       match (Segment.get_position p s) with
         | L -> rev_parse f rt p ; f s ; rev_parse f lt p
         | _ -> rev_parse f lt p ; f s ; rev_parse f rt p
      end
  | E -> ()

let rec iter f bsp = match bsp with
  | N(s,lt,rt) -> f s ; iter f lt ; iter f rt
  | E -> ()


  let rec deleteElement e = function
  | x::xs when e=x -> deleteElement e xs
  | [] -> []
  | x::xs -> x :: deleteElement e xs 

let delta_min (x, ll, lr) (s,l1,l2,min) =
  let delta = abs (List.length ll - List.length lr) in
  if delta < min then (x , ll, lr, delta)
  else (s,l1,l2,min)

let equilibrage ls = 
let rec doEquilibrage sl acc =
  match sl with
  | [] -> acc
  | x::xs -> let left,right = Segment.split x (List.filter ((!=) x) ls) in 
      doEquilibrage xs (delta_min (x, left, right) acc)
in match ls with x::_ -> doEquilibrage ls (x,[],[],80000) | _ -> assert false

let rec build_bsp = function 
    | []-> E
    | l -> let (s, ll, lr, _) = equilibrage l in
            N (s, build_bsp ll, build_bsp lr)

let elem x = List.exists ((=) x)

let print_node s depth =
  Printf.sprintf "%s" ((String.make depth ' ') ^ (string_of_int s.id));;
let rec print_tree t depth = match t with 
        | E -> String.make depth ' ' ^ "E"
        | N (s, l, r) -> (print_node s depth ^ "\n\n" ^  print_tree l (depth+1) ^ print_tree r (depth+1)) ;;

(*une instance de bsp que l'on pourrait modifier pendant l'execution*)
let instanceBsp = ref E

let updateBsp b = instanceBsp := b

    
