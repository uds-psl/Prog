(* First steps *)

let a = Array.make 3 7
let v_before = a.(1)
let _ = a.(1) <- 1
let v_after = a.(1)

let a = let a = Array.make 3 1 in a.(1) <- 2; a.(2) <- 3; a

(* Conversion between arrays and lists *)

let to_list a =
  let rec loop i l =
    if i < 0 then l else loop (i-1) (a.(i)::l)
  in loop (Array.length a - 1) []

let of_list l =
  match l with
  | [] -> [||]
  | x::l ->
    let a = Array.make (List.length l + 1) x in
    let rec loop l i =
       match l with
         | [] -> a
         | x::l -> (a.(i) <- x; loop l (i + 1))
    in loop l 1

let clone a =
  if Array.length a = 0 then [||]
  else let b = Array.make (Array.length a) (a.(0)) in
    let rec loop i =
      if i < 0 then b else (b.(i) <- a.(i) ; loop (i-1))
    in loop (Array.length a - 1)

let b = clone a
let test = a = clone a
let test = Array.make 3 1 = Array.make 4 1

(* Binary search *)

type comparison = LE | EQ | GR
let comp x y : comparison =
  if x < y then LE
  else if x = y then EQ
  else GR

let bsearch a x =
  let rec loop l r =
    if l > r then None
    else let m = (l+r) / 2 in
      match comp x a.(m) with
      | LE -> loop l (m-1)
      | EQ -> Some m
      | GR -> loop (m+1) r
  in loop 0 (Array.length a - 1)

let test = bsearch [|2;3;5;7;11;13;17;19;23|] 19

(* Array reversal *)

let swap a i j : unit =
  let x = a.(i) in
  a.(i) <- a.(j); a.(j) <- x

let reverse a =
  let rec loop i j =
    if i > j then ()
    else (swap a i j; loop (i+1) (j-1))
  in loop 0 (Array.length a - 1)

let a = [|2;3;5;7;11;13;17;19;23|]
let test = (reverse a; a)

(* Selection sort *)

let min a : int =
  let r = Array.length a - 1 in
  let rec loop k j =
    if k >= r then j
    else if a.(k+1) < a.(j) then loop (k+1) (k+1)
    else loop (k+1) j
  in loop 0 0

let ssort a : unit =
  let r = Array.length a - 1 in
  let rec min k j : int =
    if k >= r then j
    else if a.(k+1) < a.(j) then min (k+1) (k+1)
    else min (k+1) j
  in let rec loop i : unit =
       if i >= r then ()
       else (swap a (min i i) i; loop (i+1))
  in loop 0

(* Quicksort *)

let qsort a =
  let partition l r =
    swap a ((l + r) / 2) r;
    let x = a.(r) in
    let rec loop i j =
      if i > j
      then (swap a i r; i)
      else if a.(i) < x then loop (i+1) j
      else if a.(j) >= x then loop i (j-1)
      else (swap a i j; loop (i+1) (j-1))
    in loop l (r-1)   
  in let rec qsort' l r =
    if l >= r then ()
    else let m = partition l r in
      qsort' l (m-1); qsort' (m+1) r
  in qsort' 0 (Array.length a - 1)

let a = [|3;5;7;2;11;23;13;2;17;19;23|]
let test = (qsort a; a)
let a = Array.init 100000 (fun k -> -k)
let test = (qsort a; a)

let ensure_median a l r =
  let m = (l + r) / 2 in
  let (x,y,z) = (a.(l), a.(m), a.(r)) in
  if x <= z && z <= y || y <= z && z <= x then ()
  else if x <= y && y <= z || z <= y && y <= x then swap a m r
  else swap a l r

let partition a l r =
  ensure_median a l r;
  let x = a.(r) in
  let rec loop i j =
    if i > j
    then (swap a i r; i)
    else if a.(i) < x then loop (i+1) j
    else if a.(j) >= x then loop i (j-1)
    else (swap a i j; loop (i+1) (j-1))
  in loop l (r-1)
    
let qsort a =
  let rec qsort' l r =
    if l >= r then ()
    else let m = partition a l r in
      qsort' l (m-1); qsort' (m+1) r
  in qsort' 0 (Array.length a - 1)

let a = Array.init 10000 (fun k -> -k)
let test = (qsort a; a)

(* Equality *)

let a = Array.make 2 1
let b = Array.make 2 1
let test2 = (a = b)               (* true *)
let test1 = (a == b)              (* false *)
let test3 = (a.(1) <- 2; a=b)     (* false *)
let test4 = (a.(1) <- 1; a=b)     (* true *)

(* Order *)

let test = invalid_arg "1"  + invalid_arg "2"
let test = (invalid_arg "1", invalid_arg "2")
let test = (invalid_arg "1"; invalid_arg "2")

(* Reference cells *)

let c = ref 0
let next () = (c := !c + 1; !c)
let test1 = next ()
let test2 = next ()
let test3 = (next (), next ())
let test4 = next () + next () + next ()

let next = let c = ref 0 in
  fun () -> (c := !c + 1; !c)
let test1 = next ()
let test2 = next ()
let test3 = (next (), next ())
let test4 = next () + next () + next ()

let nextSquare = let c = ref 0 in
  fun () -> let n = !c in c:= n+1 ; n*n
let test0 = nextSquare ()
let test1 = nextSquare ()
let test2 = nextSquare ()
let test3 = nextSquare ()
