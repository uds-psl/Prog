type var = string
type con = Bcon of bool | Icon of int
type op  = Add | Sub | Mul | Leq
type exp = Var of var | Con of con
         | Oapp of op * exp * exp
         | Fapp of exp * exp
         | If of exp * exp * exp
         | Lam of var * exp
         | Let of var * exp * exp
         | Letrec of var * var * exp * exp

type ('a,'b) env = ('a * 'b) list
let empty : ('a,'b) env = []
let update (env : ('a,'b) env) a b : ('a,'b) env = (a,b) :: env
let rec lookup (env : ('a,'b) env) a =  match env with
  | (a',b) :: env -> if a = a' then Some b else lookup env a
  | [] -> None

type value = Bval of bool | Ival of int
           | Closure of var * exp * (var, value) env
           | Rclosure of var * var * exp * (var, value) env

let eval_op o v1 v2 = match o, v1, v2 with
  | Add, Ival n1, Ival n2 -> Ival (n1 + n2)
  | Sub, Ival n1, Ival n2 -> Ival (n1 - n2)
  | Mul, Ival n1, Ival n2 -> Ival (n1 * n2)
  | Leq, Ival n1, Ival n2 -> Bval (n1 <= n2)
  | _, _ , _ -> failwith "op app: ill-typed arguments"
                                 
let rec eval env e : value = match e with
  | Var x ->
    begin match lookup env x with
      | Some v -> v
      | None -> failwith ("variable" ^ x ^ "unbound")
    end
  | Con (Bcon b) -> Bval b
  | Con (Icon n) -> Ival n
  | Oapp (o,e1,e2) -> eval_op o (eval env e1) (eval env e2)
  | Fapp (e1,e2) -> eval_fun (eval env e1) (eval env e2)
  | If (e1,e2,e3) ->
    begin match eval env e1 with
      | Bval b -> eval env (if b then e2 else e3)
      | _ -> failwith "if: boolean expected"
    end
  | Lam (x,e) -> Closure (x,e,env)
  | Let (x,e1,e2) -> eval (update env x (eval env e1)) e2
  | Letrec (f,x,e1,e2) -> eval (update env f (Rclosure (f,x,e1,env))) e2
and eval_fun v1 v2 = match v1 with
  | Closure (x,e,env) -> eval (update env x v2) e
  | Rclosure (f,x,e,env) -> eval (update (update env f v1) x v2) e
  | _ -> failwith "fun app: function expected"

let test = eval empty
    (Letrec ("fac", "a",
             Lam ("n",
                  If (Oapp (Leq, Var "n", Con (Icon 1)), Var "a",
                      Fapp (Fapp (Var "fac", Oapp (Mul, Var "n", Var "a")),
                            Oapp (Sub, Var "n", Con (Icon 1))))),
             Fapp (Fapp (Var "fac", Con (Icon 1)), Con (Icon 4))))

(* Expression evaluating to 10! *)
let test = eval empty
    (Letrec ("fac", "a",
             Lam ("n",
                  If (Oapp (Leq, Var "n", Con (Icon 1)), Var "a",
                      Fapp (Fapp (Var "fac", Oapp (Mul, Var "n", Var "a")),
                            Oapp (Sub, Var "n", Con (Icon 1))))),
             Fapp (Fapp (Var "fac", Con (Icon 1)), Con (Icon 10))))

(* Expression evaluating to a closure *)
let test = eval empty
    (Letrec ("fac", "a",
             Lam ("n",
                  If (Oapp (Leq, Var "n", Con (Icon 1)), Var "a",
                      Fapp (Fapp (Var "fac", Oapp (Mul, Var "n", Var "a")),
                            Oapp (Sub, Var "n", Con (Icon 1))))),
             Fapp (Var "fac", Con (Icon 1))))

(* A famous diverging expression *)
(*
let test = eval empty
    (Let("omega",Lam("x",Fapp(Var"x",Var"x")), Fapp(Var"omega",Var"omega")))
*)
