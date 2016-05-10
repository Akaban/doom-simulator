open Segment
open List

type t = E | N of Segment.t * t * t 

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


let rec build_bsp sl = match sl with
  | (t::ts) -> let (left,right) = Segment.split t ts in (*equilibrage (build_bsp left) t (build_bsp right)*) N(t,build_bsp left, build_bsp right)
  | [] -> E

let rec deleteElement e = function
  | x::xs when e=x -> deleteElement e xs
  | [] -> []
  | x::xs -> x :: deleteElement e xs 

let build_bspWithPivot s sl =
  let slwos = deleteElement s sl in
  build_bsp (s::slwos)

(*une instance de bsp que l'on pourrait modifier pendant l'execution*)
let instanceBsp = ref E

let updateBsp b = instanceBsp := b

    
