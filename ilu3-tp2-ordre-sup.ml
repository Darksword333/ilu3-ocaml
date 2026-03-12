let hd l =
  match l with
  | [] -> failwith "Erreur Liste vide"
  | head::rest -> head 
    
let tl l = 
  match l with
  | [] -> failwith "Erreur Liste vide"
  | rest::tail -> tail
    
let rec tete mat =
  match mat with
  | [] -> []
  | liste::resteMatrice -> (hd liste)::(tete resteMatrice)
                     
let rec reste mat = 
  match mat with
  | [] -> []
  | []::resteMatrice -> mat
  | liste::resteMatrice -> (tl liste) :: (reste resteMatrice) 
                                                      
let rec trans mat =
  match mat with
  | [] -> [] 
  | [] :: _ -> []
  | mat -> (tete  mat) :: (trans (reste mat))
      
let rec map fct liste =
  match liste with
  | [] -> []
  | head::resteListe -> (fct head) :: (map fct resteListe)
                          (*TYPE map : *) 
                            
let tete2 mat = 
  map hd mat 
    
let reste2 mat = 
  map tl mat
                                      
let ligzero n = 
  if n < 0 then failwith "Erreur Nombre Négatif" else
    let rec ligrec n =
      match n with 
      | 0 -> []
      | _ -> 0::(ligrec (n-1))
    in ligrec n
      
let zero n =
  let ligne = ligzero n in
  map (fun _ -> ligne) ligne
  
let rec unite n = 
  match n with 
  | 0 -> []
  | n -> (1 :: (ligzero (n-1))) :: map (fun l -> 0::l) (unite (n-1))
           
let rec map2 binfct l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | _::_, [] -> failwith "Erreur l1 plus grand que l2"
  | [], _::_ -> failwith "Erreur l2 plus grand que l1"
  | head1::restel1, head2::restel2 -> (binfct head1 head2) :: (map2 binfct restel1 restel2)

let somlig l1 l2 =
  map2 (fun a b -> a+b) l1 l2

    
let add mat1 mat2 =
  map2 somlig mat1 mat2
    
let rec prodligcol l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x1::rest1, x2::rest2 -> (x1 * x2) + prodligcol rest1 rest2
  | _ -> failwith "Taille différente"
           
let prodligtmat ligne colonne =
  map (fun col -> prodligcol ligne col) colonne
                          
let prod mat1 mat2 =
  map (fun lig -> prodligtmat lig mat2) mat1
                                                            
let create f n =
  let rec aux i acc =
    if i <= 0 then acc
    else aux (i - 1) (f i :: acc)
  in
  aux n []
    
let couples n =
  create (fun i -> create (fun j -> (i, j)) n) n 
    
let zero2 n =
  create (fun _ -> create (fun _ -> 0) n) n
    
let unite2 n =
  create (fun i -> create (fun j -> if i = j then 1 else 0) n) n