(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Abdelkader-Mahamat Djamal & Bryce Tichit *)
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


let rec build_bsp = function
  | [] -> E
  | (l::ls) -> let left, right = Segment.split l ls in N(l,build_bsp left,build_bsp right)

let elem x = List.exists ((=) x)

let print_node s depth =
  Printf.sprintf "%s" ((String.make depth ' ') ^ (string_of_int s.id));;
let rec print_tree t depth = match t with 
        | E -> String.make depth ' ' ^ "E"
        | N (s, l, r) -> (print_node s depth ^ "\n\n" ^  print_tree l (depth+1) ^ print_tree r (depth+1)) ;;

(*une instance de bsp que l'on pourrait modifier pendant l'execution*)
let instanceBsp = ref E

let updateBsp b = instanceBsp := b

    
