let fst (x,y) = x
let snd (x,y) = y
let swap (x,y) = (y,x)

let car f (x,y) = f x y
let cas f x y = f (x,y)

let rec iter f n x = if n < 1 then x else iter f (n - 1) (f x)

let succ x = x + 1
let add x y = iter succ y x
let mul x y = iter (add y) x 0
let pow x y = iter (mul x) y 1

let rec fib n = if n < 2 then n else fib (n - 2) + fib (n - 1)

let pred n = fst (iter (fun (x,y) -> (y, y + 1)) n (0,0)) 
let fib' n  = fst (iter (fun (a,b) -> (b, a + b)) n (0,1))
let fac n  = snd (iter (fun (n,a) -> (n + 1, (n + 1) * a)) n (0,1))

(* fib' is faster than fib, check for 42 *)

let rec forall m n f : bool =
  m > n || f m && forall (m + 1) n f
  
let prime x =
  (x > 1) && forall 2 (x - 1) (fun k -> x mod k > 0) 
  
let rec first f k =
  if f k then k
  else first f (k + 1)

let next_prime x = first prime (x + 1)
let nth_prime n = iter next_prime n 2
    
let prime' x = x > 1 && 
               let k = first (fun k -> k * k >= x || x mod k = 0) 2
               in k * k > x

let test  = prime' 479001599
(* let test  = prime 479001599    (* slower *) *)

(* 
The prime number 87178291199 is too large for TryOcaml 
but ok for a stand alone OCaml interpreter on a 64 bit machine.
The difference is execution time between prime' and prime is drastic.
let test  = prime' 87178291199
let test  = prime 87178291199
*)
