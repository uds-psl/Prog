let rec length l =
  match l with
  | [] -> 0
  | x :: l -> 1 + length l

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l1 -> x :: append l1 l2

let rec rev l =
  match l with
  | [] -> []
  | x :: l -> append (rev l) [x]

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l1 -> rev_append l1 (x :: l2)

let rev l = rev_append l []

let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l

let rec seq m n =
  if m > n then []
  else m :: seq (m + 1) n

let cons x l = x :: l

let rec mem x l =
  match l with
  | [] -> false
  | y :: l -> (x = y) || mem x l

let rec count x l =
  match l with
  | [] -> 0
  | y :: l -> if x = y then 1 + count x l else count x l

let rec exists p l =
   match l with
  | [] -> false
  | x :: l -> p x || exists p l
 
let hd l =
  match l with
  | [] -> failwith "hd"
  | x :: _ -> x

let tl l =
  match l with
  | [] -> failwith "tl"
  | _ :: l -> l

let rec nth l n =
  match l with
  | [] -> failwith "nth"
  | x :: l -> if n < 1 then x else nth l (n-1)

let rec filter p  l =
  match l with
  | [] -> []
  | x :: l -> if p x then x :: filter p l else filter p l

let rec eq (p: 'a -> 'a -> bool) l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x::l1, y::l2 -> p x y || eq p l1 l2
  | _, _ -> false

let rec eq (p: 'a -> 'a -> bool) l1 l2 =
  match l1 with
  | [] ->
    begin match l2 with
      | [] -> true
      | _ :: _ -> false
    end
  | x::l1 ->
    begin match l2 with
      | [] -> false
      | y::l2 -> p x y && eq p l1 l2
    end

(* Generalized patterns *)

let rec pow x n =
  match n with
  | 0 -> 1
  | _ -> x * pow x (n - 1)

let test l =
  match l with
  | 1 :: 2 :: _ -> true
  | _ -> false

let test l =
  match l with
  | [] -> false
  | x :: l -> x = 1 &&
              match l with
              | [] -> false
              | y :: _ -> y = 2

(* Insertion sort *)
                          
let rec insert x l =
  match l with
  | [] -> [x]
  | y :: l -> if x <= y then x :: y :: l else y :: insert x l
                                                
let rec isort l =
  match l with
  | [] -> []
  | x :: l -> insert x (isort l)
                                                
let gisort p l =
  let rec insert x l =
    match l with
    | [] -> [x]
    | y :: l -> if p x y then x :: y :: l else y :: insert x l
  in
  List.fold_right insert l []

let test = gisort (<=) [5;3;2;2;4]
let test = gisort (>=) [5;3;2;2;4]
let test = gisort (<=) [true;false;false;true]
let test = gisort (>=) [true;false;false;true]

let sort l = gisort (<=) l
let sort l = gisort (fun (x,_) (y,_) -> x <= y) l

let test = sort [(3,7); (1,8); (3,23); (1,5)]

let rec lex p l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _::_, [] -> false
  | x1::l1, x2::l2 -> p x1 x2 && if p x2 x1 then lex p l1 l2 else true

let test = lex (<=) [-1] [-1;-2]
let test = [-1] <= [-1;-2]

(* Sublists *)
                   
let rec pow l =
  match l with
  | [] -> [[]]
  | x :: l -> pow l @ List.map (fun l -> x :: l) (pow l)

let rec pow l k =
  if k < 1 then [[]]
  else
    match l with
    | [] -> []
    | x :: l -> pow l k @ List.map (fun l -> x :: l) (pow l (k-1))

let rec is_sublist l1 l2 =
  match l1, l2 with
  | l1, [] -> l1 = []
  | [], y::l2 -> true
  | x::l1, y::l2 -> is_sublist (x::l1) l2 || 
                    (x = y) && is_sublist l1 l2 

(* Prime factorization *)

let rec first f k =
  if f k then k
  else first f (k + 1)

let rec prime_fac x =
  if x < 2 then []
  else let k = first (fun k -> x mod k = 0) 2 in
    if k = x then [x]
    else k :: prime_fac (x / k)

let test = prime_fac 735
(* let test = prime_fac 479001599 (* slow *) *)
    
let rec prime_fac' k x =
  if k * k > x then [x]
  else if x mod k = 0 then k :: prime_fac' k (x / k)
  else prime_fac' (k + 1) x

let prime_fac x = if x < 2 then [] else prime_fac' 2 x

let test = prime_fac' 2 735
let test = prime_fac' 2 479001599
(* let test = prime_fac' 2 87178291199 (* Too large for Try OCaml *) *)

(* Environments *)

type ('a,'b) env = ('a * 'b) list

let rec lookup l a =
  match l with
  | [] -> invalid_arg "lookup"
  | (a',b) :: l -> if a = a' then b else lookup l a

let test = lookup [("x",3); ("y",7); ("z",2)] "y"

let rec update l a b =
  match l with
  | [] -> [(a,b)]
  | (a',b') :: l -> if a = a' then (a,b) :: l
    else (a',b') :: update l a b

let rec update (l: ('a,'b) env)  a b : ('a,'b) env =
  match l with
  | [] -> [(a,b)]
  | (a',b') :: l -> if a = a' then (a,b) :: l
    else (a',b') :: update l a b

let test = update (update (update (update [] "x" 3) "y" 7) "z" 2) "y" 13

let rec lookup' l a =
  match l with
  | [] -> None
  | (a',b) :: l -> if a = a' then Some b else lookup' l a

let test = lookup' [("x",3); ("y",7); ("z",2)] "y"

let bound l a =
  match lookup' l a with
  | Some _ -> true
  | None -> false

let test = bound [("x",3); ("y",7); ("z",2)] "y"
