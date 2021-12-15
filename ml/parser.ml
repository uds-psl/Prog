type const   = BCON of bool | ICON of int
type token   = ADD | SUB | MUL | LP | RP | EQ | LEQ | ARR
             | IF | THEN | ELSE | LAM | LET | IN | REC
             | CON of const | VAR of string

let code = Char.code
let num c = code c - code '0'
let digit c = code '0' <= code c && code c <= code '9'
let lc_letter c = code 'a' <= code c && code c <= code 'z'
let uc_letter c = code 'A' <= code c && code c <= code 'Z'
let whitespace c = match c with
  | ' ' | '\n' |  '\t' -> true
  | _ -> false

let lex s : token list =
  let get i = String.get s i in
  let getstr i n = String.sub s (i-n) n in
  let exhausted i = i >= String.length s in
  let verify i c = not (exhausted i) && get i = c in
  let rec lex i l =
    if exhausted i then List.rev l
    else match get i with
      | '+' -> lex (i+1) (ADD::l)
      | '*' -> lex (i+1) (MUL::l)
      | '=' -> lex (i+1) (EQ::l)
      | '(' -> lex (i+1) (LP::l)
      | ')' -> lex (i+1) (RP::l)
      | '<' -> if verify (i+1) '='
          then lex (i+2) (LEQ::l)
          else failwith "lex: '=' expected"
      | '-' -> if verify (i+1) '>'
          then lex (i+2) (ARR::l)
          else lex (i+1) (SUB::l)
      | c when whitespace c -> lex (i+1) l
      | c when digit c -> lex_num (i+1) (num c) l
      | c when lc_letter c -> lex_id (i+1) 1 l
      | c -> failwith "lex: illegal character"
  and lex_num i n l =
    if exhausted i then lex_num' i n l
    else let c = get i in
      if digit c then lex_num (i+1) (10*n + num c) l
      else lex_num' i n l
  and lex_num' i n l = lex i (CON (ICON n)::l)
  and lex_id i n l =
    if exhausted i then lex_id' i n l
    else match get i with
      | '\'' | '_' -> lex_id (i+1) (n+1) l
      | c -> if lc_letter c || uc_letter c || digit c
        then lex_id (i+1) (n+1) l
        else lex_id' i n l
  and lex_id' i n l = match getstr i n with
    | "if" -> lex i (IF::l)
    | "then" -> lex i (THEN::l)
    | "else" -> lex i (ELSE::l)
    | "fun" -> lex i (LAM::l)
    | "let" -> lex i (LET::l)
    | "in" -> lex i (IN::l)
    | "rec" -> lex i (REC::l)
    | "false" -> lex i (CON (BCON false)::l)
    | "true" -> lex i (CON (BCON true)::l)
    | s -> lex i (VAR s::l)
  in lex 0 []

let fac_string =
  "let rec fac a = fun n ->
    if n <= 1 then a else fac (n*a) (n-1) 
   in fac 1 5"

let fac_tokens = lex fac_string

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

let verify t l =  match l with
  | [] -> failwith "verify: no token"
  | t'::l -> if t'=t then l else failwith "verify: wrong token"

let rec exp l : exp * token list = match l with
  | IF::l ->
      let (e1,l) = exp l in
      let (e2,l) = exp (verify THEN l) in
      let (e3,l) = exp (verify ELSE l) in
      (If(e1,e2,e3), l)
  | LAM::VAR x::ARR::l ->
      let (e,l) = exp l in (Lam (x,e), l)
  | LET::VAR x::EQ::l ->
      let (e1,l) = exp l in
      let (e2,l) = exp (verify IN l) in
      (Let (x,e1,e2), l)
  | LET::REC::VAR f::VAR x::EQ::l ->
      let (e1,l) = exp l in
      let (e2,l) = exp (verify IN l) in
      (Letrec (f,x,e1,e2), l)
  | l -> cexp l
and cexp l = let (e,l) = sexp l in cexp' e l
and cexp' e1 l = match l with
  | LEQ::l -> let (e2,l) = sexp l in (Oapp(Leq,e1,e2), l)
  | l -> (e1,l)
and sexp l = let (e,l) = mexp l in sexp' e l
and sexp' e1 l = match l with
  | ADD::l -> let (e2,l) = mexp l in sexp' (Oapp(Add,e1,e2)) l
  | SUB::l -> let (e2,l) = mexp l in sexp' (Oapp(Sub,e1,e2)) l
  | l -> (e1,l)
and mexp l = let (e,l) = aexp l in mexp' e l
and mexp' e1 l = match l with
  | MUL::l -> let (e2,l) = aexp l in aexp' (Oapp(Mul,e1,e2)) l
  | l -> (e1,l)
and aexp l = let (e,l) = pexp l in aexp' e l
and aexp' e1 l = match l with
  | CON _ :: _ | VAR _ :: _ | LP :: _  ->
      let (e2,l) = pexp l in aexp' (Fapp(e1,e2)) l
  | l -> (e1,l)
and pexp l = match l with
  | CON (BCON b)::l -> (Con (Bcon b), l)
  | CON (ICON n)::l -> (Con (Icon n), l)
  | VAR x::l -> (Var x, l)
  | LP::l -> let (e,l) = exp l in (e, verify RP l)
  |  _ -> failwith "pexp"

let test = exp (lex fac_string)
