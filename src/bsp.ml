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

let rec parseLeft f bsp p = match bsp with
  | N(s,lt,rt) ->
      begin 
        match (Segment.get_position p s) with
        | L -> parseLeft f lt p
        | _ -> parseLeft f lt p ; f s ; parseLeft f rt p 
      end
  | E -> ()


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


let rec hauteur = function
  | E -> 0
  | N(_,lt,rt) -> 1 + max (hauteur lt) (hauteur rt)

let creation l s r = N(s,l,r)

let rotationGauche = function
  | N(p,a,N(q,b,c)) -> N(q,N(p,a,b),c)
  | t -> t

let rotationDroite = function
  | N(q,N(p,a,b),c) -> N(p,a,N(q,b,c))
  | t -> t

let id = fun x -> x

let rec equilibrageduTerTer = function
  | E -> E
  | N(s,l,r) as t -> 
    let hl,hr = hauteur l, hauteur r in
    let rotation =
    if hl > hr + 1 then rotationDroite
    else rotationGauche in
    if hl=hr then t else 
    rotation (N(s,equilibrageduTerTer l,equilibrageduTerTer r))

let equilibrage l s r =
  let creation l s r = N(s,l,r) in
  let hl, hr = hauteur l, hauteur r in
  if hl > hr+1 then begin
   match l with
    | N(ls, ll, lr) when hauteur ll >= hauteur lr 
      -> creation ll ls (creation lr s r)
    | N(ls, ll, N(lrs, lrl, lrr)) ->
       creation (creation ll ls lrl) lrs (creation lrr s r)
    | _ -> assert false
  end else if hr > hl +1 then begin
    match r with
    | N(rs,rl,rr) when hauteur rr >= hauteur rl
      -> creation (creation l s rl) rs rr
    | N(rs,N(rls,rll,rlr),rr) 
      -> creation (creation l s rll) rls (creation rlr rs rr)
    | _ -> assert false
  end
  else creation l s r

let rec leftMost = function
  | E -> raise Not_found
  | N(s,E,_) -> s
  | N(_,l,_) -> leftMost l

let rec del_leftMost = function
  | E -> raise Not_found
  | N(_,E,r) -> r
  | N(s,l,r) -> 
      equilibrage (del_leftMost l) s r

let fusion t1 t2 = match t1, t2 with
  | E, t
  | t, E -> t
  | _, _ -> equilibrage t1 (leftMost t2) (del_leftMost t2)

let rec ajout x = function
  | E -> N(x,E,E)
  | N(s,l,r) -> begin
    match (Segment.get_position x.porig s) with
      | L -> equilibrage (ajout x l) s r
      | _ -> equilibrage l s (ajout x r) 
                end


let flip f x y = f y x


let rec build_bspSimple sl splitF = match sl with
  | (t::ts) -> let (left,right) = splitF t ts in N(t,build_bspSimple left splitF, build_bspSimple right splitF)
  | [] -> E
 
type treeDir = L | R
type prevTreeDir = treeDir option

let rec deleteElement e = function
  | x::xs when e=x -> deleteElement e xs
  | [] -> []
  | x::xs -> x :: deleteElement e xs 



let build_bspWithPivot s sl =
  let slwos = deleteElement s sl in
  build_bspSimple (s::slwos) Segment.split


let build_bsp sl = 
  let bsp1 = equilibrageduTerTer (build_bspSimple sl splitWithoutId) in
  let pivot = match bsp1 with
    | N(p,_,_) -> p
    | E -> assert false in
  equilibrageduTerTer (build_bspWithPivot pivot sl)


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
(*let rec build_bsp = function
  | x::xs -> let l,_= Segment.split x xs in
             ajout l x (build_bsp xs)
  | [] -> E
*)

let print_node s depth =
  Printf.sprintf "%s" ((String.make depth ' ') ^ (string_of_int s.id));;
let rec print_tree t depth = match t with 
        | E -> String.make depth ' ' ^ "E"
        | N (s, l, r) -> (print_node s depth ^ "\n\n" ^  print_tree l (depth+1) ^ print_tree r (depth+1)) ;;

let build_bspWithPivot s sl =
  let slwos = deleteElement s sl in
  build_bsp (s::slwos)

(*une instance de bsp que l'on pourrait modifier pendant l'execution*)
let instanceBsp = ref E

let updateBsp b = instanceBsp := b

    
