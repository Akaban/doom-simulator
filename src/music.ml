open Graphics

type lenNote = Noire | Croche | Blanche | DCroche | Ronde
type partition = (int*lenNote) array
type partitionTempo = (partition*int)

let new_partition p t = p, t 

(*toutes les notes*)
let do1 = 65 and do2=131 and do3 = 262 and do4=523 and dod1=69 
and dod2 = 139 and dod3 = 277 and dod4 = 554 
and  re1 = 73 and re2 = 147 and re3 = 294 and re4 = 587 
and  mib1 = 78 and mib2 = 156 and mib3 = 311 and mib4 = 622 
and  mi1 = 82 and mi2 = 165 and mi3 = 330 and mi4 = 659 
and  fa1 = 87 and fa2 = 175 and fa3 = 349 and fa4 = 698 
and  fad1 = 92 and fad2 = 185 and fad3 = 370 and fad4 = 740 
and  sol1 = 98 and sol2 = 196 and sol3 = 392 and sol4 = 784 
and  sold1 = 104 and sold2 = 208 and sold3 = 415 and sold4 = 830 
and  la1=110 and la2 = 220 and la3=440 and la4 = 880 
and  sib1 = 117 and sib2 = 233 and sib3 = 466 and sib4 = 932 
and  si1 = 123 and si2 = 247 and si3 = 494 and si4 = 988 
and  do5 = 1046;;

let toInt = function
  | Noire -> 0
  | Croche -> 1
  | Blanche -> 2
  | DCroche -> 3
  | Ronde -> 4

let len t = [|60000/t;120000/t;30000/t;15000/t;240000/t|]


let playT partition tempo =
  let tempoD = len tempo in
  for i=0 to Array.length partition - 1 do
    let note, temps = partition.(i) in sound note (tempoD.(toInt(temps)))
  done

let play (partition,tempo) = playT partition tempo

let tetris =
  let partition=[| si3,Noire ; fad3,Croche ;sol3,Croche;la3,Noire;sol3,Croche;fad3,Croche;
  mi3,Noire;mi3,Croche;sol3,Croche;si3,Noire;la3,Croche;sol3,Croche;fad3,Noire;fad3,Croche;
  sol3,Croche;la3,Noire;si3,Noire;sol3,Noire;mi3,Noire;mi3,Noire |] in new_partition partition 144


let () =
  open_graph " 100x100";
  play tetris ; wait_next_event [Key_pressed] ;
  exit 0;;

