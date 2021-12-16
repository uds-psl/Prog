type ty = Int | Bool | Arrow of ty * ty
type var = string
type con = Bcon of bool | Icon of int
type op  = Add | Sub | Mul | Leq
type exp = Var of var | Con of con
         | Oapp of op * exp * exp
         | Fapp of exp * exp
         | If of exp * exp * exp
         | Lamty of var * ty * exp
         | Let of var * exp * exp
         | Letrecty of var * var * ty * ty * exp * exp

 let check_op o t1 t2 : ty = match o, t1, t2 with
  | Add, Int, Int -> Int
  | Sub, Int, Int -> Int
  | Mul, Int, Int -> Int
  | Leq, Int, Int -> Bool
  | _, _ , _ -> failwith "op app: ill-typed arguments"

let check_fun t1 t2 : ty = match t1 with
  | Arrow (t11,t12) -> if t11 = t2 then t12
    else failwith "fun app: wrong argument type"
  | _ -> failwith "fun app: function expected"

type ('a,'b) env = ('a * 'b) list
let empty : ('a,'b) env = []
let update (env : ('a,'b) env) a b : ('a,'b) env = (a,b) :: env
let rec lookup (env : ('a,'b) env) a =  match env with
  | (a',b) :: env -> if a = a' then Some b else lookup env a
  | [] -> None
                              
let rec check env e : ty = match e with
  | Var x ->
    begin match lookup env x with
      | Some t -> t
      | None -> failwith ("variable" ^ x ^ "unbound")
    end
  | Con (Bcon b) -> Bool
  | Con (Icon n) -> Int
  | Oapp (o,e1,e2) -> check_op o (check env e1) (check env e2)
  | Fapp (e1,e2) -> check_fun (check env e1) (check env e2)
  | If (e1,e2,e3) ->
    begin match check env e1, check env e2, check env e3 with
      | Bool, t2, t3 -> if t2 = t3 then t2
        else failwith "If: then and else type not equal"
      | _, _, _  -> failwith "if: bool expected"
    end
  | Lamty (x,t,e) -> Arrow (t, check (update env x t) e)
  | Let (x,e1,e2) -> check (update env x (check env e1)) e2
  | Letrecty (f,x,t1,t2,e1,e2) ->
    let env1 = update env f (Arrow(t1,t2)) in
    if check (update env1 x t1) e1 = t2 then check env1 e2
    else failwith "let rec: declared type not matched"

let test = check empty
    (Letrecty ("fac", "a", Int, Arrow(Int,Int), 
               Lamty ("n", Int,
                      If (Oapp (Leq, Var "n", Con (Icon 1)), Var "a",
                          Fapp (Fapp (Var "fac", Oapp (Mul, Var "n", Var "a")),
                                Oapp (Sub, Var "n", Con (Icon 1))))),
               Fapp (Fapp (Var "fac", Con (Icon 1)), Con (Icon 4))))
