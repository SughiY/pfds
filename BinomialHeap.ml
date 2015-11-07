module type Comparable =
sig
  type t 
  val compare : t -> t -> int
end


module BinomialHeap(Element:Comparable) = struct

  let eq x y = ((Element.compare x y) = 0)
  let lt x y = ((Element.compare x y) < 0)
  let gt x y = ((Element.compare x y) > 0)
  let leq x y = ((Element.compare x y) <= 0)

  exception EMPTY
  exception HAVE_SAME_ONE

  type tree = Node of int * Element.t * tree list
  type heap = tree list

  let rank = function | Node (r, x, c) -> r
  let root = function | Node (r, x, c) -> x
  let link t1 t2 = match (t1, t2) with
    | (Node (r, x1, c1) , Node(_, x2, c2)) ->
      if leq x1 x2
      then Node (r + 1, x1, t2::c1)
      else Node (r + 1, x2, t1::c2)

  let rec insTree t = function
    | [] -> [t]
    | h::r as l ->
      if rank t < rank h then t::l else insTree (link t h) r

  let insert e t = insTree (Node (0, e, [])) t

  let list_of_tree l =
    let rec aux h l = match l with
    |[] -> h
    |t::r -> aux (insert t h) r
    in
    aux [] l

  let rec merge l1 l2 = match (l1, l2) with
    | (l1, []) -> l1
    | ([], l2) -> l2
    | (t1::s1, t2::s2) ->
      if rank t1 < rank t2 then t1::(merge s1 l2)
      else if rank t2 < rank t1 then t2 :: (merge l1 s2)
      else insTree (link t1 t2) (merge s1 s2)
  let rec removeMinTree = function
    | [] -> raise EMPTY 
    | [t] -> (t, [])
    | t::ts -> let (t1, ts1) = removeMinTree ts in
      if (root t) <= (root t1) then (t, ts) else (t1, t::ts1)
  let findMin ts = let (t, _) = removeMinTree ts in root t
  let findMin2 = function
    | [] -> raise EMPTY
    | t::r as ts -> 
    let rec aux m = function
      | [] -> m
      | t::r -> let r_t = root t in if r_t <= m then aux r_t r else aux m r in
    aux (root t) ts

  let deleteMin ts = let (Node(_, x, ts1), ts2) = removeMinTree ts in
    merge (List.rev ts1) ts2
end

module IntBinomialHeap = BinomialHeap(String);;

open IntBinomialHeap;;

let a = list_of_tree (List.map (fun x -> string_of_int x) [1;3;6;2;4;7;9]);;
let b = list_of_tree (List.map (fun x -> string_of_int x) [9;7;4;2;6;3;1]);;

findMin a;; 
findMin2 b;; 
