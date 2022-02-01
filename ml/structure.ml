(* Cells *)

module type CELL = sig
  type 'a cell
  val make : 'a -> 'a cell
  val get  : 'a cell -> 'a
  val set  : 'a cell -> 'a -> unit
end

module Cell : CELL = struct
  type 'a cell = 'a array
  let make x = Array.make 1 x
  let get c = c.(0)
  let set c x = c.(0) <- x
end

let c = Cell.make 5
let x = Cell.get c
let y = Cell.set c 7; Cell.get c

let enum = let c = Cell.make 0 in
  fun () -> let x = Cell.get c in Cell.set c (x+1); x

(* Stacks *)

module type STACK = sig
  type 'a stack
  val make   : 'a -> 'a stack
  val push   : 'a stack -> 'a -> unit
  val pop    : 'a stack -> unit
  val top    : 'a stack -> 'a
  val height : 'a stack -> int
end

module Stack : STACK = struct
  type 'a stack = 'a list ref
  exception Empty
  let make x = ref [x]
  let push s x = s:= x :: !s
  let pop s = match !s with
    | [] -> raise Empty
    | x::l -> s:= l
  let top s = match !s with
    | [] -> raise Empty
    | x::l ->x
  let height s = List.length (!s)
end
      
let s = Stack.make 3
let test1 = Stack.top s
let test2 = Stack.push s 7; Stack.top s
let test3 = Stack.height s
let test4 = Stack.pop s; Stack.pop s; Stack.pop s; Stack.pop s

(* Queues *)

module type QUEUE = sig
  type 'a queue
  val make   : 'a -> 'a queue
  val insert : 'a queue -> 'a -> unit
  val remove : 'a queue -> unit
  val first  : 'a queue -> 'a
  val length : 'a queue -> int
end

module Queue : QUEUE = struct
  type 'a queue = 'a list ref
  exception Empty
  let make x = ref [x]
  let insert q x = q := !q @ [x]
  let remove q = match !q with
    | [] -> raise Empty
    | _::l -> q := l
  let first q = match !q with
    | [] -> raise Empty
    | x::_ -> x
  let length q = List.length (!q)
end

(* Bounded Stack  *)

module type BSTACK = sig
  val empty : unit -> bool
  val full  : unit -> bool
  val push  : int -> unit
  val pop   : unit -> unit
  val top   : unit -> int
end

module S : BSTACK = struct 
  let size = 100
  let a = Array.make size 0
  let h = ref 0
  exception Empty
  exception Full
  let empty () = !h = 0
  let full () = !h = size
  let push x = if full() then raise Full else (a.(!h) <- x; h:= !h + 1)
  let pop () = if empty() then raise Empty else h:= !h - 1                                  
  let top () = if empty() then raise Empty else a.(!h -1)
end

(* Queues *)

module type BQUEUE = sig
  val empty  : unit -> bool
  val full   : unit -> bool
  val insert : int -> unit
  val remove : unit -> unit
  val first  : unit -> int
end

module Q : BQUEUE = struct
  let maxSize = 3
  let a = Array.make maxSize 0
  let s = ref 0
  let l = ref 0
  exception Empty
  exception Full
  let empty () = !l = 0
  let full () = !l = maxSize
  let pos x = x mod maxSize
  let insert x = if full() then raise Full
    else (a.(pos(!s + !l)) <- x; l:= !l + 1)
  let remove () = if empty() then raise Empty
    else (s:= pos (!s + 1); l:= !l - 1)                                
  let first () = if empty() then raise Empty else a.(!s)
end

(* Heap *)

module type HEAP = sig
  exception Address
  exception Full
  type address = int
  type index = int
  val alloc   : int -> address
  val get     : address -> index -> int
  val set     : address -> index -> int -> unit
  val release : address -> unit
end

(*module H : HEAP = struct*)
  let maxSize = 1000
  let h = Array.make maxSize (-1)
  let s = ref 0  (* size of heap *)
  exception Address
  exception Full
  type address = int
  type index = int
  let alloc n = if n < 1 then raise Address
    else if !s + n > maxSize then raise Full
    else let a = !s in s := !s + n; a
  let check a = if a < 0 || a >= !s then raise Address else a
  let get a i = h.(check(a+i))
  let set a i x = h.(check(a+i)) <-x
  let release a = s := check a
(*end
open H*)

let alloc' l =
  let a = alloc (List.length l) in
  let rec loop l i = match l with
    | [] -> a
    | x::l -> h.(a+i) <- x; loop l (i+1)
  in loop l 0

let rec putlist l = match l with
  | [] -> -1
  | x::l -> alloc' [x; putlist l]

let rec getlist a =
  if a = -1 then [] 
  else  get a 0 :: getlist (get a 1)

let test = getlist (putlist [2;3;-1;4])
