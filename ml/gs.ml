let a = 2 * 3 + 2
let b = 5 * a
let a = 5
let c = a + b

let square x = x * x

let a = square 5

let pow x = square (square (square x))

let pow' x = 
  let a = x * x in
  let b = a * a in
  b * b

let abs x = if x < 0 then -x else x

let max x y : int = if x <= y then y else x

let max3 x y z = max (max x y) z

let max x y z : int =
  let a = if x <= y then y else x in
  if a <= z then z else a

let test (x : int) y z = if x <= y then y <= z else false

let rec pow x n =
  if n < 1 then 1
  else x * pow x (n - 1)

let rec digit_sum x = 
  if x < 10 then x 
  else digit_sum (x / 10) + (x mod 10)

let rec rev' x a = 
  if x <= 0 then a
  else rev' (x / 10) (10 * a + x mod 10)
let rev x = rev' x 0

let rec gcd x y =
  if y < 1 then x
  else gcd y (x mod y)

let rec my_div x y = if x < y then 0 else 1 + my_div (x - y) y
let rec my_mod x y = if x < y then x else my_mod (x - y) y

let rec sqrt' k n =
  if n < square (k + 1) then k
  else sqrt' (k + 1) n
let sqrt n = sqrt' 0 n
    
let rec first f k =
  if f k then k
  else first f (k + 1)

let test n k = n < square (k + 1)
let sqrt n = first (test n) 0

let sqrt n = first (fun k -> n < square (k + 1)) 0

let curt n = first (fun k -> n < pow (k + 1) 3) 0

let div x y = first (fun k -> x < (k + 1) * y) 0

let test = fun n k -> n < (k + 1) * (k + 1)

let test = fun n -> (fun k -> n < (k + 1) * (k + 1))

let rec pow' x n a =
  if n < 1 then a
  else pow' x (n -1) (x * a)

let sec (h,m,s) = 3600 * h + 60 * m + s
let hms x = 
  let h = x / 3600 in
  let m = (x mod 3600) / 60 in
  let s = x mod 60 in
  (h,m,s)

let add = (+)
let layz_or = (||)
