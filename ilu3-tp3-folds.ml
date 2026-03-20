let sum_sq l = 
  List.fold_left (+) 0 (List.map (fun x -> x*x) l)
    
let sum_sq_trec l = 
  let rec aux acc = function
    | [] -> acc
    | head :: tail -> aux (acc + head*head) tail in
  aux 0 l

let inclus l1 l2 =
  List.for_all (fun x -> List.exists (fun y -> x=y) l2) l1
    
let sup2 l1 l2 =
  List.for_all (fun x -> List.exists (fun y -> 
      List.exists (fun z -> x < y && x > z) l2) l2) l1
  
let sum l =
  List.fold_left (+) 0 l
    
let pair_sup l =
  (List.fold_left (+) 0 (List.filter (fun x -> x mod 2 = 0) l)) > 
  (List.fold_left (+) 0 (List.filter (fun x -> x mod 2 <> 0) l)) 
  
let split l = 
  let rec aux l pair impair = 
    match l with
    | [] -> (pair, impair)
    | head :: tail -> if head mod 2 = 0 then aux tail (head::pair) impair
        else aux tail pair (head::impair) in
  aux l [] []
  
let pair_sup_trec l =
  let splitlist = split l in
  match splitlist with
  | l1, l2 -> (sum l1) > (sum l2)
                        
                        