let len = 8000
let l_asc = List.init len (fun n -> n)
let l_desc = List.init len (fun n -> len-1-n)

(* List Reversal *)

let rec rev l = match l with
  | [] -> []
  | x::l -> rev l @ [x]

let test = rev (List.init 12000 (fun n -> n))

let rec rev_app l a = match l with
  | [] -> a
  | x::l -> rev_app l (x::a)

let test = rev_app (List.init 12000 (fun n -> n)) []

(* Insertion Sort *)

let rec insert x l = match l with
  | [] -> [x]
  | y :: l -> if x <= y then x :: y :: l else y :: insert x l
                                                
let rec isort l = match l with
  | [] -> []
  | x :: l -> insert x (isort l)

let test_asc = isort l_asc   (* fast *)
let test_desc = isort l_desc   (* slow *)
  
(* Tail-recursive version *)

let rec insert' x l a = match l with
  | [] -> List.rev_append a [x]
  | y :: l when x <= y -> List.rev_append a (x::y::l)
  | y :: l -> insert' x l (y::a) 
                                              
let rec isort' l a = match l with
  | [] -> a
  | x :: l -> isort' l (insert' x a [])

let test_asc = isort' l_asc []   (* slow *)
let test_desc = isort' l_desc []   (* fast *)

(* Merge sort *)
    
let rec split l l1 l2 = match l with
  | [] -> (l1,l2)
  | [x] -> (x::l1,l2)
  | x::y::l -> split l (x::l1) (y::l2)

let rec merge l1 l2 = match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x::l1, y::l2 when x <= y -> x :: merge l1 (y::l2)
  | x::l1, y::l2  -> y :: merge (x::l1) l2

let rec msort l = match l with
  | x::y::l -> let (l1,l2) = split l [x] [y] in merge (msort l1) (msort l2)
  | l -> l

let test_asc = msort l_asc
let test_desc = msort l_desc

(* Tail-recursive version *)

let rec merge l1 l2 l = match l1, l2 with
  | [], l2 -> List.rev_append l l2
  | l1, [] -> List.rev_append l l1
  | x::l1, y::l2 when x <= y -> merge l1 (y::l2) (x::l)
  | x::l1, y::l2 -> merge (x::l1) l2 (y::l)

let rec msort l = match l with
  | x::y::l -> let (l1,l2) = split l [x] [y] in merge (msort l1) (msort l2) []
  | l -> l

let test_asc = msort l_asc
let test_desc = msort l_desc

let l = List.init 100000 (fun n -> n)
let test = msort l
