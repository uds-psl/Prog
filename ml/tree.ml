type tree = A | B of tree * tree

let t = B(B(A,A), B(B(A,A),A)) 

let rec size t =
  match t with
  | A -> 1
  | B(t1,t2) -> 1 + size t1 + size t2

let test = size t

let max x y = if x<y then y else x

let rec depth t =
  match t with
  | A -> 0
  | B(t1,t2) -> 1 + max (depth t1) (depth t2)

let test = depth t

let rec mirror t =
  match t with
  | A -> A
  | B(t1,t2) -> B(mirror t2, mirror t1)

let test = mirror t


(* Balanced trees *)
    
let rec balanced t =
  match t with
  | A -> Some 0
  | B(t1,t2) -> match balanced t1, balanced t2 with
    | Some n1, Some n2 -> if n1 = n2 then Some (1 + n1) else None
    | _, _ -> None

let test = balanced t
let test = balanced (B(B(A,A), B(A,A)))

let rec btree n =
  if n < 1 then A
  else  let t = btree (n-1) in B(t,t)

let test = btree 4
let test = balanced (btree 20)

(* Prefix linearization *)

let rec pre t =
  match t with
  | A -> "A"
  | B(t1,t2) -> "B" ^ pre t1 ^ pre t2

let test = pre t

(* Postfix Linearization *)

let rec post t =
  match t with
  | A -> "A"
  | B(t1,t2) -> post t1 ^ post t2 ^ "B"

let test = post t

(* Infix linearization, left-associative *)

let rec tree t = match t with
  | A -> "A"
  | B(t1,t2) -> tree t1 ^ "B" ^ ptree t2
and ptree t = match t with
  | A -> "A"
  | t -> "(" ^ tree t ^ ")"

let rec tree t =
  let ptree t = match t with
    | A -> "A"
    | t -> "(" ^ tree t ^ ")"
  in match t with
  | A -> "A"
  | B(t1,t2) -> tree t1 ^ "B" ^ ptree t2

(* ABC-trees *)

type ctree = A | B of ctree * ctree | C of ctree * ctree

let rec ctree t = match t with
  | C(t1,t2) -> ctree t1 ^ "C" ^ btree t2
  | t -> btree t
and btree t = match t with
  | B(t1,t2) -> btree t1 ^ "B" ^ ptree t2
  | t -> ptree t
and ptree t = match t with
  | A -> "A"
  | t -> "(" ^ ctree t ^ ")"

let t = C(C(A,A), C(B(A,A), A))
let test = ctree t

(* Abstract expressions *)

type var = string
type con = Bcon of bool | Icon of int
type op  = Add | Sub | Mul
type exp = Var of var | Con of con
         | Oapp of op * exp * exp
         | Fapp of exp * exp
         | If of exp * exp *exp
         | Lam of var * exp
         | Let of var * exp * exp
         | Letrec of var * var * exp * exp

let int2string x =
  let digit2string n = match n with
    | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3"
    | 4 -> "4" | 5 -> "5" | 6 -> "6" | 7 -> "7"
    | 8 -> "8" | 9 -> "9" | _ -> "error"
  in
  let rec nat2string n =
    if n < 10 then digit2string n
    else nat2string (n/10) ^ digit2string (n mod 10)
  in
  if x < 0 then "-" ^ nat2string (-x) else nat2string x
                     
