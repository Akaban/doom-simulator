(* Projet PFA 2015-2016
 * Université Paris Sud L3
 * Par Abdelkader-Mahamat Djamal & Bryce Tichit *)
open Graphics

let bmpMatrix path l h=
    let cin=open_in path in
    let m=Array.make_matrix l h 0 in
    let b,v,r =ref 0, ref 0, ref 0 in
    seek_in cin 54; (*début des données byte 54*)
    for j=0 to (h-1) do      
        for i=0 to (l-1) do
            b:=input_byte cin;
            v:=input_byte cin;
            r:=input_byte cin;
            m.(i).(j)<- rgb !r !v !b
        done
    done;
    close_in cin;
    m;;
