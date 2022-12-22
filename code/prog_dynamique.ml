let plsuffixe c1 c2 = 
  let maxlen = ref 0  in
  let indicec1 = Array.make 2 0 in 
  let indicec2 = Array.make 2 0 in 
  let l1 = String.length c1  in 
  let l2 = String.length c2  in 
  let tab = Array.make_matrix (l1+1) (l2+1) 0 in 

  for i = 1 to l1 do 
    for j = 1 to l2 do 
      
      if c1.[i-1] == c2.[j-1] then tab.(i).(j) <- tab.(i-1).(j-1) + 1 ;
      if tab.(i).(j) > !maxlen then (maxlen := tab.(i).(j);
                                     indicec1.(1) <- i-1;
                                     indicec1.(0) <- indicec1.(1) - !maxlen + 1 ; 
                                     indicec2.(1) <- j-1;
                                     indicec2.(0) <- indicec2.(1) - !maxlen  + 1;);
    done ; 
  done;
  
  !maxlen, indicec1, indicec2 ;;



let () = 
  assert(plsuffixe "ANANAS" "BANANE" = (4, [|0;3|], [|1;4|]));
  assert(plsuffixe "ABAB" "BABA" = (3, [|0; 2|], [|1; 3|]));
  assert(plsuffixe "CAMARCHE" "AHNONMARCHE" = (6, [|2; 7|], [|5; 10|]));;
  
    

