module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module LeftistHeap(Elem:Comparable) = struct
  exception Empty
    exception Same_elem

  type heap = E | T of int * Elem.t * heap * heap  

  let rank = function 
      | E -> 0
      | T (r ,_ ,_ ,_ ) -> r

  let makeT x a b =
    if rank a >= rank b
    then T(rank b + 1, x, a, b)
    else T(rank a + 1, x, b, a)

  let rec merge m n = match (m, n) with
    | (h, E) -> h
    | (E, h) -> h 
    | (T (_, x, a1, b1) as h1, (T (_, y, a2, b2) as h2)) ->
      if (Elem.compare x y) < 0
      then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge b2 h1)

 let insert_merge x h = merge (T (1, x, E, E)) h

  let rec insert x t =
    try
      match t with
      | E -> T (1, x, E, E)
      | T (_, y, left, right ) ->
        match (Elem.compare x y) with
        | n when n < 0 -> makeT x E t
        | 0 -> raise Same_elem
        | _ -> makeT y left (insert x right)
    with
      Same_elem -> t

  let rec creat_l_heap f = function 
    | [] -> E
    | h::t -> (f h (creat_l_heap f t))

  let create_merge l = creat_l_heap insert_merge l 
  let create_insert l = creat_l_heap insert l
end;;

module IntLeftTree = LeftistHeap(String);;

open IntLeftTree;;

let l = ["a";"b";"d";"g";"z";"e";"c"];;
let lh = create_merge l;;
let li = create_insert l;;

let h = ["c";"e";"z";"g";"d";"b";"a"];;
let hh = create_merge h;;
let hi = create_insert h;;

