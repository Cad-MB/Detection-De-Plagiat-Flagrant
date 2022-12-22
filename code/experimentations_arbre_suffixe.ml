open Prog_dynamique ;;
open Arbre_suffixe ;;

let read_base name_file =
  let channel = open_in name_file in
  let rec read_file () = 
    try 
      let data =  (input_line channel ) in
      data :: (read_file ())
    with End_of_file ->  close_in channel ; []
  in
  List.rev (read_file()) ;;



let time p s1 s2: float =
  let t = Unix.times() in
  let ps = p s1 s2 in
  let t2 = Unix.times() in 
  Printf.printf "Temps d'excution en %f secondes\n" (t2.tms_utime -. t.tms_utime);
  ps;
  (t2.tms_utime -. t.tms_utime);; 



let rec tab_time_pro_dynamique (s1: string) (s2: string) (l: int array) = 
  let tab = Array.make (Array.length l + 1) 0. in
  print_string "Taille de la 1er chaine : ";
  print_int (String.length s1) ;
  print_string "\n";
  print_string "Taille de la 2eme chaine : ";
  print_int (String.length s2) ;
  print_string "\n";
  for i = 0 to (Array.length l)-1 do 
    if String.length s2 < l.(i) then tab.(i) <- (time plsuffixe ((String.sub s1 0 l.(i))) s2) 
    else  tab.(i) <- (time plsuffixe ((String.sub s1 0 l.(i))) ((String.sub s2 0 l.(i)))); 
  done;
  tab.(Array.length l) <- (time plsuffixe (s1) (s2));;



let rec tab_time_arbre_compresse (s1: string) (s2: string) (l: int array) = 
  let tab = Array.make (Array.length l + 1) 0. in
  print_string "Taille de la 1er chaine : ";
  print_int (String.length s1) ;
  print_string "\n";
  print_string "Taille de la 2eme chaine : ";
  print_int (String.length s2) ;
  print_string "\n";
  for i = 0 to (Array.length l)-1 do 
    if String.length s2 < l.(i) then tab.(i) <- (time souschainescommunescompress ((String.sub s1 0 l.(i))^"#") (s2^"#") ) 
    else tab.(i) <- (time souschainescommunescompress ((String.sub s1 0 l.(i))^"#") ((String.sub s2 0 l.(i))^"#"));
  done;
  tab.(Array.length l) <- (time souschainescommunescompress (s1^"#") (s2^"#"));; 



let s0 = (read_base "Donnees_test/donnee0.txt");;
let s1 = (read_base "Donnees_test/donnee1.txt");;
let s2 = (read_base "Donnees_test/donnee2.txt");;
let s3 = (read_base "Donnees_test/donnee3.txt");;
let s4 = (read_base "Donnees_test/donnee4.txt");;
let s5 = (read_base "Donnees_test/donnee5.txt");;
let s6 = (read_base "Donnees_test/donnee6.txt");;
let l = [| 10; 20; 50; 75; 100; 125; 150; 200; 350; 400; 500 |];; 


(*---Donnee0 et Donnee1---*)
print_string "\n";;
print_string "Donnee0 Donnee1 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s1) l;; 
print_string "\n";;
print_string "Donnee0 Donnee1 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s1) l;; 
print_string "\n";;

(*---Donnee0 et Donnee2---*) 
print_string "Donnee0 Donnee2 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s2) l;; 
print_string "\n";;
print_string "Donnee0 Donnee2 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s2) l;;
print_string "\n";;

(*---Donnee0 et Donnee3---*)
print_string "Donnee0 Donnee3 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s3) l;; 
print_string "\n";;
print_string "Donnee0 Donnee3 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s3) l;;
print_string "\n";;

(*---Donnee0 et Donnee4---*)
print_string "Donnee0 Donnee4 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s4) l;; 
print_string "\n";;
print_string "Donnee0 Donnee4 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s4) l;;
print_string "\n";;

(*---Donnee0 et Donnee5---*)
print_string "Donnee0 Donnee5 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s5) l;; 
print_string "\n";;
print_string "Donnee0 Donnee5 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s5) l;;
print_string "\n";;

(*---Donnee0 et Donnee6---*)
print_string "Donnee0 Donnee6 Prog Dynamique \n";;
let tab_pro_dynamique = tab_time_pro_dynamique (String.concat "" s0) (String.concat "" s6) l;; 
print_string "\n";;
print_string "Donnee0 Donnee6 ArbreSuffixeCompress \n";;
let tab_arbre_compresse = tab_time_arbre_compresse (String.concat "" s0) (String.concat "" s6) l;;
print_string "\n";;