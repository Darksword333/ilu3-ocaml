let rec doublons l =
  match l with
  | [] -> [] 
  | x :: rest -> let result = doublons rest in 
      if List.mem x rest && not (List.mem x result) then x::result else result

let rec get a0 l =
  match l with
  | [] -> failwith "Erreur"
  | (x, y) :: rest -> if x = a0 then y else get a0 rest

let rec put a0 b0 l =
  match l with
  | [] ->(a0,b0)::[]
  | (x,y)::rest -> if x = a0 then (x,b0)::rest else (x,y) :: put a0 b0 rest
          
  
module ENV_LIST : tENV_LIST = struct
  type ('x, 'v) env = ('x * 'v) list
  let empty = []
  let get = get
  let put = put
end

module ENV_FUN : tENV_FUN = struct
  type ('x, 'v) env = 'x -> 'v
    
  let empty = fun _ -> failwith "Erreur"
  let get x env  = env x
  let put x v env = 
    fun k -> if k = x then v else env k
end 

let regle_if expr =
  match expr with
  | If(c, BConst true, BConst false) -> c
  | If(c1, BConst true, c2) -> Call(Or, c1, c2)
  | If(c1, c2, BConst false) -> Call(And, c1, c2)
  | _ -> expr
    
let rec apply regle e =
  match e with 
  | IConst _ | BConst _ | Var _ -> regle e

  | If(c, e1, e2) -> 
      let c' = apply regle c in
      let e1' = apply regle e1 in
      let e2' = apply regle e2 in
      regle (If(c', e1', e2'))

  | Let(x, e1, e2) -> 
      let e1' = apply regle e1 in
      let e2' = apply regle e2 in
      regle (Let(x, e1', e2'))

  | Call(op, e1, e2) -> 
      let e1' = apply regle e1 in
      let e2' = apply regle e2 in
      regle (Call(op, e1', e2'))
        
module EVAL : tEVAL = functor (E : tENV) -> struct
  type value = Vint of int | Vbool of bool 
                   
  let eval_op op v1 v2 = 
    match (op, v1, v2) with
    | (Add, Vint n1, Vint n2) -> Vint (n1 + n2)
    | (Leq, Vint n1, Vint n2) -> Vbool (n1 <= n2)
    | (And, Vbool n1, Vbool n2) -> Vbool (n1 && n2)
    | (Or, Vbool n1, Vbool n2) -> Vbool (n1 || n2)
    | _ -> failwith "Erreur"
  
  let rec eval env e = 
    match e with
    | IConst n -> Vint n
    | BConst b -> Vbool b
    | Var x -> E.get x env 
    | If(c, e1, e2) ->
        (match eval env c with
         | Vbool true -> eval env e1
         | Vbool false -> eval env e2
         | Vint _ -> failwith "Erreur") 
    | Call(op, e1, e2)  -> eval_op op (eval env e1) (eval env e2)
    | Let(x, e1, e2) -> 
        let v1 = eval env e1 in 
        let env_etendu = E.put x v1 env in
        eval env_etendu e2
    
end

module TYPECHECK : tTYPECHECK = functor (E : tENV) -> struct 
  
  let typeof_op op = 
    match op with
    | Leq -> (Int, Int, Bool)
    | Or -> (Bool, Bool, Bool)
    | And -> (Bool, Bool, Bool)
    | Add -> (Int, Int, Int)
             
  let rec typeof env e =
    match e with
    | IConst _ -> Int
    | BConst _ -> Bool 
    | Var x -> E.get x env
    | If(c, e1, e2) ->
        let tc = typeof env c in
        let t1 = typeof env e1 in
        let t2 = typeof env e2 in
        if tc = Bool && t1 = t2 then t1
        else failwith "Erreur de typage" 
    | Call(op, e1, e2) ->
        let t1 = typeof env e1 in
        let t2 = typeof env e2 in
        let (t1', t2', res) = typeof_op op in
        if t1 = t1' && t2 = t2' then res
        else failwith "Erreur typage"
    | Let(x, e1, e2) -> 
        let v1 = typeof env e1 in 
        let env_etendu = E.put x v1 env in
        typeof env_etendu e2
  
end
             