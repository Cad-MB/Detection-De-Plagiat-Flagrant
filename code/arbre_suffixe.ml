type 'a suffixtree = 
    Node of int * string * 'a suffixtree list 


let vide = Node(0,"",[]) 
    
let getenfant noeud = 
  match noeud with 
  | Node(i,s,tab) -> tab 
  
let explode wd = 
  let rec exp i acc =
    if i = -1 then acc else exp (i-1) ((String.make 1 wd.[i])::acc) in 
  exp (String.length wd - 1) []

let appartient mot noeud = 
  let enfant = getenfant noeud in 
  let rec parcours_enfant e = 
    match e with 
    | Node(i,s,l)::q -> if mot = s then (true,Node(i,s,l))
        else parcours_enfant q 
    | [] -> (false,Node(0,"",[]))
  in parcours_enfant enfant ;;


let filter mot l = 
  List.filter (fun x -> 
      match x with 
      | Node(i,s,l) -> if s = mot then false else true ) l 
            

let rec print_list_enfant noeud =
  match noeud with 
  | [] -> "" 
  | Node(i,s,l)::q -> s^print_list_enfant q
  
let ajout mot arbre n = 
  let rec ajout_bis mot arbre = 
    match (mot,arbre) with 
    | [], Node(i,s,l) -> Node(i,s,l) 
    | t::q, Node(i,s,l) -> 
        let appartient_t = appartient t (Node(i,s,l)) in 
        match appartient_t with 
        | (false,_) -> (Node(i,s,ajout_bis q (Node(n,t,[]))::l))
        | (true,Node(i2,s2,l2)) -> if i2 != n then 
              (Node(i,s,(ajout_bis q (Node(n+1,s2,l2)))::filter s2 l) ) 
            else (Node(i,s,(ajout_bis q (Node(i2,s2,l2)))::filter s2 l) )
                        
  in ajout_bis (explode mot) arbre;; 


let rec arbresuffixe arbre mot n = 
  let mot_bis = (String.sub mot 1 ((String.length mot) -1)) in 
  match mot.[0] with 
  | '#' -> ajout mot arbre n
  | x -> arbresuffixe (ajout mot arbre n) mot_bis n;;

let souschaine mot1 mot2 = 
  let arbre = arbresuffixe vide mot1 1 in 
  let rec souschaine_bis mot arbre = 
    match (mot,arbre) with 
    | t::q, Node(i,s,l) -> 
        if t = "#" then true else 
          (let appartient_t = appartient t (Node(i,s,l)) in 
           match appartient_t with 
           | (false,_) -> false 
           | (true,Node(i2,s2,l2)) -> souschaine_bis q (Node(i2,s2,l2)))
    | [], Node(_,s,l) -> true
  in souschaine_bis (explode mot2) arbre ;;


let rec recup_chaine noeud = 
  match noeud with
  | [] -> ""
  | Node(i,s,l)::q -> s^recup_chaine l ;;

let recup_chaine_un noeud = 
  match noeud with 
  | [] -> "" 
  | Node(i,s,l)::q -> s
    
let rec hauteur_arbre arbre = 
  match arbre with 
  | Node(_,_,[]) -> 0 
  | Node(i,s,l) -> 1 + hauteur_arbre (List.hd l) ;;

let souschainecommunes mot1 mot2 = 
  let arbre = arbresuffixe vide mot1 1 in 
  let arbre_mots = arbresuffixe arbre mot2 2 in 
  let enfants = getenfant arbre_mots in
  let rec parcours_arbre_max e res= 
    match e with 
    | [] -> res::[]
    | Node(i,s,l)::q -> if i = 3 then parcours_arbre_max q ((Node(i,s,List.hd (parcours_arbre_max l [])))::res)
        else (parcours_arbre_max q res)
  in let solu = parcours_arbre_max enfants [] in 
  let rec plusgrand sol max noeud= 
    match sol with 
    | (Node(i,s,[]))::q -> plusgrand q max noeud 
    | (Node(i,s,l))::q -> if max < (hauteur_arbre (List.hd l)) then plusgrand q (hauteur_arbre (List.hd l)) (Node(i,s,l))
        else plusgrand q max noeud 
    | [] -> noeud 
  in recup_chaine (plusgrand (List.hd solu) 0 (Node(0,"",[]))::[]);;


let compression arbre = 
  let enfants = getenfant arbre in 
  let rec parcours_arbre e = 
    match e with 
    | [] -> [] 
    | Node(i,s,l)::q -> 
        if List.length l = 1 then parcours_arbre (Node(i,s^recup_chaine_un(getenfant (Node(i,s,l))),parcours_arbre (getenfant (List.hd l)))::parcours_arbre q) 
        else (Node(i,s,parcours_arbre l)::parcours_arbre q) 
  in Node(0,"",parcours_arbre enfants);;
    

let souschainescommunescompress mot1 mot2 =
  let arbre = compression (arbresuffixe (arbresuffixe vide mot1 1) mot2 2)  in 
  let enfants = getenfant arbre in 
  let rec parcours_arbre_max e res= 
    match e with 
    | [] -> res::[]
    | Node(i,s,l)::q -> if i = 3 then parcours_arbre_max q ((Node(i,s,List.hd (parcours_arbre_max l [])))::res)
        else (parcours_arbre_max q res)
  in let solu = parcours_arbre_max enfants [] in 
  let rec plusgrand sol max noeud= 
    match sol with 
    | (Node(i,s,[]))::q -> plusgrand q max noeud 
    | (Node(i,s,l))::q -> 
        let som = ((hauteur_arbre (List.hd l)) + (String.length s) + String.length (recup_chaine l)) in 
        if max < som 
        then plusgrand q som (Node(i,s,l))
        else plusgrand q max noeud 
    | [] -> noeud 
  in recup_chaine (plusgrand (List.hd solu) 0 (Node(0,"",[]))::[]);;
    

(*let arbre = compression (arbresuffixe (arbresuffixe vide "ANANAS#" 1) "BANANE#" 2);;
 let testcompress = souschainescommunescompress "BANANE#" "ANANAS#";;
 let testcompress = souschainescommunescompress "ABAB#" "BABA#";;
 let arbre = compression (arbresuffixe (arbresuffixe vide "JESUISGRANDAVEC#" 1) "TUESGRANDPETIT#" 2);;

 let testcompress = souschainescommunescompress "CAFONCTIONNE#" "CENESTPASPOSSIBLECAFONCTIONNE#";;*)

let arbre = compression (arbresuffixe vide "ANANAS#" 1) ;;

let compare_mot mot mot1 = 
  let rec compare mot mot1 = 
    match (mot,mot1) with 
    | [], x -> x
    | x, [] -> x
    | t1::q1, t2::q2 -> if t1 = t2 then compare q1 q2 
        else t1::q1
  in compare mot mot1;;
             

let ajout_mot_compress mot arbre = 
  let rec ajout_bis mot arbre = 
    match arbre with 
    | Node(i,s,[]) -> Node(i,s,Node(0,mot,[])::[])
    | Node(i,s,l) -> 
        let enfant = getenfant arbre in 
        let rec parcours_enfant e = 
          match e with 
          | Node(i,s,l)::q -> if s.[0] == mot.[0] then 
                let newmot = compare_mot (explode s) (explode mot) in 
                let racine = (String.sub s 0 ((String.length s) - (List.length newmot))) in
                let s_enfant = (String.sub s ((String.length racine)) ((String.length s) - (String.length racine))) in 
                let s_enfant2 = (String.sub mot ((String.length racine)) ((String.length mot) - (String.length racine))) in
                let n_enfant = Node(0,s_enfant,l) in 
                let n_enfant2 = Node(0,s_enfant2,[]) in 
                Node(i,racine,n_enfant::n_enfant2::[])::q 
              else Node(i,s,l)::parcours_enfant q 
          | [] -> Node(0,mot,[])::[]
        in Node(i,s,parcours_enfant enfant);
  in ajout_bis mot arbre;;

let rec arbresuffixescompresse arbre mot = 
  let mot_bis = (String.sub mot 1 ((String.length mot) -1)) in 
  match mot.[0] with 
  | '#' -> ajout_mot_compress mot arbre 
  | x -> arbresuffixescompresse (ajout_mot_compress mot arbre) mot_bis 
  
           


    
    
  
