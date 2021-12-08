(* Lexing *)

type tree = A | B of tree * tree | C of tree * tree
type token = AT | BT | CT | LP | RP

let lex s =
  let n = String.length s in
  let rec lex i l =
    if i >= n then List.rev l
    else match String.get s i with
      | 'A' -> lex (i+1) (AT::l)
      | 'B' -> lex (i+1) (BT::l)
      | 'C' -> lex (i+1) (CT::l)
      | '(' -> lex (i+1) (LP::l)
      | ')' -> lex (i+1) (RP::l)
      | ' ' | '\n' |  '\t' -> lex (i+1) l
      | _ -> failwith "lex: illegal character"
  in lex 0 []

let test = lex " ABA C (ABA) "

let explode s = List.init (String.length s) (String.get s)
let implode l = List.fold_right (fun c s -> String.make 1 c ^ s) l ""

let test = explode "Saarbrücken"
let test = implode (explode "Saarbrücken")

let test = List.map Char.code (explode "Saarbrücken")
let test = Char.chr (Char.code 'a')

(* Recursive Descent Parsing *)

let rec tree l =  match l with
  | AT::l -> (A,l)
  | BT::l ->
    let (t1,l) = tree l in
    let (t2,l) = tree l in
    (B(t1,t2), l)
  | _ -> failwith "tree"

let verify c l =  match l with
  | [] -> failwith "verify: no token"
  | c'::l -> if c'=c then l else failwith "verify: wrong token"

let rec tree l = match l with
  | AT::l -> (A,l)
  | BT::l ->
    let (t1,l) = tree l in
    let (t2,l) = tree l in
    (B(t1,t2), l)
  | LP::l ->
    let (t,l) = tree l in
    (t, verify RP l)
  | _ -> failwith "tree"

let test = tree (lex "B(BAA)BABAA")

(* Left Associativity *)

let rec tree l =
  let (t,l) = ptree l in tree' t l
and tree' t l = match l with
  | BT::l -> let (t',l) = ptree l in tree' (B(t,t')) l
  | _ -> (t,l)
and ptree l = match l with
  | AT::l -> (A,l)
  | LP::l -> let (t,l) = tree l in (t, verify RP l)
  | _ -> failwith "tree"

let test = tree (lex "ABABABA")
let test = tree (lex "(((ABA)BABA))")
let test = tree (lex "(ABA)BA")

(* Right Associativity *)

let rec tree l =
  let (t,l) = ptree l in tree' t l
and tree' t l = match l with
  | BT::l ->
    let (t',l) = ptree l in
    let (t'',l) = tree' t' l in
    (B(t,t''), l)
  | _ -> (t,l)
and ptree l = match l with
  | AT::l -> (A,l)
  | LP::l -> let (t,l) = tree l in (t, verify RP l)
  | _ -> failwith "tree"

let test = tree (lex "ABABABA")

(* Precedence and left associativity *)

let rec tree l =
  let (t,l) = btree l in tree' t l
and tree' t l = match l with
  | CT::l -> let (t',l) = btree l in tree' (C(t,t')) l
  | _ -> (t,l)
and btree l =
  let (t,l) = ptree l in btree' t l
and btree' t l = match l with
  | BT::l -> let (t',l) = ptree l in btree' (B(t,t')) l
  | _ -> (t,l)
and ptree l = match l with
  | AT::l -> (A,l)
  | LP::l -> let (t,l) = tree l in (t, verify RP l)
  | _ -> failwith "tree"

let test = tree (lex "ACABACABABABA")

(* Precedence, left associativity, and juxtaposition *)

let rec tree l =
  let (t,l) = btree l in tree' t l
and tree' t l = match l with
  | CT::l -> let (t',l) = btree l in tree' (C(t,t')) l
  | _ -> (t,l)
and btree l =
  let (t,l) = ptree l in btree' t l
and btree' t l = match l with
  | AT::_ | LP::_ -> let (t',l) = ptree l in btree' (B(t,t')) l
  | _ -> (t,l)
and ptree l = match l with
  | AT::l -> (A,l)
  | LP::l -> let (t,l) = tree l in (t, verify RP l)
  | _ -> failwith "tree"

let test = tree (lex "ACAA(AA)CAAAA")
let test = tree (lex "ACAA(AA)BCAAAA")
let test = tree (lex "AAA(AA)(A(AA)A)")

(* Postfix parsing *)

let rec depost l1 l2 = match l1, l2 with
  | [], l2 -> Some l2
  | AT::l1, l2 -> depost l1 (A::l2)
  | BT::l1, t2::t1::l2 -> depost l1 (B(t1,t2)::l2)
  | CT::l1, t2::t1::l2 -> depost l1 (C(t1,t2)::l2)
  | _, _ -> None

let test = depost (lex "AABABAABBAC") []

(* Mini-Ocaml *)

type con   = Bcon of bool | Icon of int
type tok   = Add | Sub | Mul | LP | RP | Eq | Leq | Arr
           | If | Then | Else | Lam | Let | Letrec | In
           | Con of con | Var of string

let num c = Char.code c - Char.code '0'
