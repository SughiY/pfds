module DEQUE = struct
  exception EMPTY

type 'a queue = 'a list * 'a list
let split l =
  let rec aux s l n = match (n, l) with
  | (_, []) | (0, _) -> (s, l)
  | (n, h::t) -> aux (h::s) t (n - 1) in
  aux [] l ((List.length l) / 2 + 1)

let empty = ([], [])

let cons x = function
  | ([], r) -> ([x], r)
  | (f, r) -> (x :: f, r)

let head = function
  | ([], _) -> raise EMPTY
  | (h::t, _) -> h

let rec tail = function
  | ([],[]) -> raise EMPTY
  | ([], r) -> tail (split r)
  | (h::t, r) -> (t, r)

let 


end
