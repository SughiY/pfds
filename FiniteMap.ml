
module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module FiniteMap(Key:Comparable) = struct
exception NotFound
  type 'a map = Nil | Bind of Key.t * 'a * 'a map
  let empty = Nil
  let bind k e m = Bind (k, e, m)
  let rec lookup k = function
      | Nil -> raise NotFound
      | Bind (key, e, m) ->
        if key = k then e else lookup k m
end;;

module StringMap = FiniteMap(String);;

open StringMap;;

let m1 = bind "a" "Car" Nil;;
let m2 = bind "b" "Fuck" m1;;
let hasCar  = lookup "a" m2;; 

