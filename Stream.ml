module STREAM = struct
  type 'a streamCell = Nil | Cons of 'a * 'a stream
  and 'a stream = 'a streamCell lazy_t

  let rec (++) lhs rhs = match lhs with
    | lazy Nil -> rhs
    | lazy (Cons (x, s)) -> lazy (Cons(x, (s ++ rhs)))

  let rec take n s = match (n, s) with
    | (0, _) | (_, lazy Nil) -> s
    | (_, lazy (Cons(x, s))) -> lazy (Cons(x, (take (n - 1) s)))

  let drop n s = 
    let rec aux n s = match (n, s) with
      | (0, _) | (_, lazy Nil) -> s
      | (_, lazy (Cons(x, s))) -> aux (n - 1) s
    in
    aux n s

  let reverse s =
    let rec aux r = function
      | lazy Nil -> r
      | lazy (Cons(x, s)) -> aux (lazy (Cons(x, r))) s
    in
    aux (lazy Nil) s

  let cons_l xs = match xs with | (x, s) -> lazy (Cons(x, s))

  let insert_sort xs = 
    let rec insert x = function
      | lazy Nil as s -> cons_l (x, s)
      | lazy (Cons(s, xs)) as suiv ->
        if x > s then cons_l (s, (insert x xs))
        else cons_l (x, suiv)
    in
    let rec aux l_aux = function
    | lazy Nil -> l_aux
    | lazy (Cons(x, s)) -> aux (insert x l_aux) s
    in
    aux (lazy Nil) xs

  let rec stream_of_list = function
    | [] -> lazy Nil
    | t::r -> lazy (Cons(t, stream_of_list r))

  let force_to l n =
    let rec aux l_aux l n = match (n, Lazy.force l) with
    | (0, _) | (_, Nil) -> List.rev l_aux
    | (_, (Cons (x, s))) -> aux (x::l_aux) s (n - 1)
    in
    aux [] l n
end

open STREAM;;

let s = stream_of_list [1;5;8;3;7;2;4];;
let sorted_s = insert_sort s;;
let list_s = force_to sorted_s 5;;

