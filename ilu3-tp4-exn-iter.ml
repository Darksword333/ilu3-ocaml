type prop = CaseOccupe of elem 
          | PinceVide
          | ConjProp of prop * prop 
          | NegProp of prop
            
let contient elem = 
  match elem  with
  | Obstacle -> CaseOccupe (elem)
  | Objet -> CaseOccupe (elem)
  | Rien -> NegProp (CaseOccupe (elem))
   
let pince_est_vide = 
  PinceVide
    
let andp prop1 prop2 =
  ConjProp (prop1, prop2)
    
let notp prop =
  NegProp (prop)
  
let some_prop = 
  andp (contient Objet) pince_est_vide
    
    (* Type de it_prop :*)
let it_prop fContient fVide fAnd fNot element =
  let rec process elem =
    match elem with
    | CaseOccupe e -> fContient e
    | PinceVide -> fVide
    | ConjProp (p1, p2) -> fAnd p1 p2 (process p1) (process p2)
    | NegProp p -> fNot p (process p)
  in process element
    
type etat = Etat of plateau 
                    * (int * int) (* pos Robot*)
                    * etat_pince 
                    * (int * int) (*vec Direction*)
  
  (* Non c'est un type produit car l'on veut avoir un ET et non un OU*)

let mk_etat plat pos etat vec =
  Etat(plat, pos, etat, vec)
    
let get_plateau etat =
  match etat with
  | Etat(plat, _,_,_) -> plat
    
let get_position etat =
  match etat with
  | Etat(_,pos ,_,_) -> pos
    
let get_pince etat =
  match etat with
  | Etat(_,_,pince ,_) -> pince    

let get_direction etat =
  match etat with
  | Etat(_,_,_,dir) -> dir

let eval etat prop =
  it_prop
    (fun elem -> (get_plateau etat) (get_position etat) = elem)
    (get_pince etat = Vide)
    (fun _ _ res1 res2 -> res1 && res2)
    (fun _ res -> not res)
    prop

let prendreCase element =
  match element with
  | Objet -> Rien
  | _ -> failwith "Case Vide"
           
let prendreA plat pos =
  let element = prendreCase (plat pos) in
  fun pos2 -> if pos2 = pos then element else (plat pos2)
                                              
let etatPrendre e =
  let plt = get_plateau e in
  let pos = get_position e in
  let pince = get_pince e in
  let dir = get_direction e in
  match pince, plt pos with
  | Vide, Objet -> mk_etat (prendreA plt pos) pos Pleine dir
  | _ -> failwith "Impossible de prendre"

           
let poserCase e = 
  match e with
  | Rien -> Objet
  | _ -> failwith "Case Occupée"
           
let poserA plat pos =
  let element = poserCase (plat pos) in
  fun pos2 -> if pos2 = pos then element else (plat pos2)
                                              
let etatPoser e =
  let plt = get_plateau e in
  let pos = get_position e in
  let pince = get_pince e in
  let dir = get_direction e in
  match pince, plt pos with
  | Pleine, Rien -> mk_etat (poserA plt pos) pos Vide dir
  | _ -> failwith "Impossible de poser"
           
let etatTourner e =
  let plt = get_plateau e in
  let pos = get_position e in
  let pince = get_pince e in
  let dir = 
    match get_direction e with
    | (1, 0) -> (0, 1)
    | (0, 1) -> (-1, 0)
    | (-1, 0) -> (0, -1)
    | (0, -1) -> (1, 0)
    | _ -> failwith "Erreur direction"
  in 
  mk_etat plt pos pince dir 
    
let etatAvancer e =
  let plt = get_plateau e in
  let pos = get_position e in
  let pince = get_pince e in
  let dir = get_direction e in
  let newPos = 
    match pos, dir with
    | (x, y), (a, b) -> (x+a, y+b) 
  in 
  if plt newPos <> Obstacle then mk_etat plt newPos pince dir 
  else failwith "Erreur case pleine"

type commande = Prendre
              | Poser
              | Tourner
              | Avancer
              | ExecCond of commande * prop
              | ExecList of commande list

let prendre =
  Prendre
    
let poser =
  Poser
    
let tourner =
  Tourner
  
let avancer =
  Avancer
    
let if_prop cmd prop = 
  ExecCond (cmd, prop)
    
let seq cmds =
  ExecList (cmds)

let rec executer e cmd =
  match cmd with
  | Prendre -> etatPrendre e
  | Poser -> etatPoser e
  | Tourner -> etatTourner e
  | Avancer -> etatAvancer e
  | ExecCond (com, prop) -> if eval e prop then executer e com else e
  | ExecList (cmds) -> 
      let rec exec_liste e = function
        | [] -> e
        | c::reste -> exec_liste (executer e c) reste
      in exec_liste e cmds
  
let rec executer2 e = function
  | ExecList (cmds) -> List.fold_left executer2 e cmds
  | cmd -> executer e cmd

let rec first_occ pred l =
  match l with
  | [] -> raise Impossible
  | elem :: reste -> 
      if (pred elem) then elem
      else first_occ pred reste

let first_occ_opt pred l =
  try Some (first_occ pred l) with
  | Impossible -> None

