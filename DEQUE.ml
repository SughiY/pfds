module DEQUE = struct
  exception EMPTY

type 'a queue = 'a list * 'a list
let rsplit l =
  let rec aux s l n = match (n, l) with
  | (_, []) | (0, _) -> (List.rev l, s)
  | (n, h::t) -> aux (h::s) t (n - 1) in
  aux [] l (List.length l / 2)

let lsplit l =
  let rec aux s l n = match (n, l) with
  | (_, []) | (0, _) -> (List.rev s, List.rev l)
  | (n, h::t) -> aux (h::s) t (n - 1) in
  aux [] l ((List.length l + 1) / 2)

let empty = ([], [])

let cons x = function
  | ([], r) -> ([x], r)
  | (f, r) -> (x :: f, r)

let head = function
  | ([], _) -> raise EMPTY
  | (h::_, _) -> h

let rec tail = function
  | ([], _) -> raise EMPTY
  | ([x], r) -> rsplit r
  | (h::t, r) -> (t, r)

let snoc q a = match q with
  | ([], []) -> ([a], [])
  | (f, []) -> (f, [a])
  | (f, r) -> (f, a::r) 

let last = function
  | ([], []) -> raise EMPTY
  | (h::_, []) -> h
  | (_, h::t) -> h

let rec init = function
  | ([], []) -> raise EMPTY
  | (f, []) -> init (lsplit f)
  | (f, [x]) -> lsplit f 
  | (f, h::t) -> (f, t)

end

open DEQUE;;

let a = lsplit [1;5;3;5;7];;
cons 2 a;;
head a;;
tail a;;
tail (tail (tail a));;

snoc a 6;;
last a;;
init a;;
init (init a);;
