#require "core";;
open Core.Std;;

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module BinaryTree(A:Comparable) = struct
  exception Same_elem

  type binaryTree = | Empty 
                    | Tree of binaryTree * A.t * binaryTree

  let leaf x = Tree (Empty, x, Empty)

  let rec member m = function
        | Empty -> false
        | Tree (left, a, right) ->
          match (A.compare m a) with
          |  n when n < 0 -> member m left
          |  0 -> true
          |  _ -> member m right

  let rec insert m t =
    try
    match t with
    | Empty -> leaf m
    | Tree (left, a, right) as tree -> 
      match (A.compare m a) with
      | n when n < 0 -> Tree (insert m left, a, right)
      | 0 -> raise Same_elem
      | _ -> Tree (left, a, insert m right)
    with
    Same_elem -> t

  let search m t =
    let rec helper x candidate = function
      | Tree (left, a, right) as tree ->
        if (A.compare x a) < 0
        then helper x candidate right
        else helper x tree right
      | Empty ->
        match candidate with
        | Empty -> Empty
        | Tree (_, a, _) -> if a = x then candidate else Empty
            in
    helper m Empty t
 let complete x d =  
   let rec helper x = function 
     | 0 -> leaf x
     | n -> let t = helper x (n - 1) in Tree (t, x, t) 
   in
   helper x d;
end;;

module IntBinaryTree = BinaryTree(Int);;

open IntBinaryTree;;

let a = leaf 1;;
let b = insert 5 (insert 3 (insert 2 a));;
let c = insert 3 b;; 

(*2.2 use the one variable to reduce the number of comparasion *)
(*2.3 use execption to return to original state*)
