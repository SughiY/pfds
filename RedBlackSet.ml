module type Comparable =
sig
  type t 
  val compare : t -> t -> int
end


module RedBlackSet(Element:Comparable) = struct

  exception EMPTY
  exception HAVE_SAME_ONE

  type color = R | B
  type elem = Element.t
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let rec member x = function
    | E -> false
    | T (_, a, y ,b) ->
      if x < y then member x a
      else if y < x then member x b
      else true

  let balance = function
    | (B, T(R, T(R, a, x, b), y, c), z, d) 
    | (B, T(R, a, x, T(R, b, y, c)), z, d) 
    | (B, a, x, T(R, T(R, b, y, c), z, d)) 
    | (B, a, x, T(R, b, y, T(R, c, z, d))) -> T (R, T (B, a, x, b), y , T (B, c, z, d))
    | (c, a, x, b) -> T (c, a, x, b) 

  let lbalance = function
    | (B, T(R, T(R, a, x, b), y, c), z, d) 
    | (B, T(R, a, x, T(R, b, y, c)), z, d) -> T (R, T (B, a, x, b), y , T (B, c, z, d))
    | (c, a, x, b) -> T (c, a, x, b) 

  let rbalance = function
    | (B, a, x, T(R, T(R, b, y, c), z, d)) 
    | (B, a, x, T(R, b, y, T(R, c, z, d))) -> T (R, T (B, a, x, b), y , T (B, c, z, d))
    | (c, a, x, b) -> T (c, a, x, b) 

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x ,E)
      |T (color, a, y, b) as s -> 
        if x < y then balance (color, ins a , y ,b)
        else if x < y then balance (color, a, y , ins b)
        else s
    in let T(_, a, y, b) = ins s
    in T(B, a, y ,b)

let insert_opt x s =
    let rec ins = function
      | E -> T (R, E, x ,E)
      |T (color, a, y, b) as s -> 
        if x < y then lbalance (color, ins a , y ,b)
        else if x < y then rbalance (color, a, y , ins b)
        else s
    in let T(_, a, y, b) = ins s
    in T(B, a, y ,b)

  let rec fromOrdList = function
    | [] -> E
    | t :: r -> insert t (fromOrdList r)

end

module StringRedBlackSet = RedBlackSet(String);;
open StringRedBlackSet;;

let t = fromOrdList ["b";"c";"d";"g";"e";"z";"h";"l";"j"];;
