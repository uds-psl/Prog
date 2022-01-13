(* Euclid *)

let rec gcd x y : int
  = if y > 0 then gcd y (x mod y) else x

let test = gcd 91 35

(* Prime factorization *)

let rec first p k =
  if p k then k else first p (k + 1)

let rec pfac x : int list =
  if x < 2 then []
  else let k = first (fun n -> x mod n = 0) 2 in
    k :: pfac (x / k)

let test = pfac 735

let rec pfac' k x : int list =
  if k * k > x then [x]
  else if x mod k = 0 then k :: pfac' k  (x / k)
  else  pfac' (k + 1) x

let test = pfac' 2 735 
