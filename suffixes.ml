let suffixes l =
  let rec aux l_aux = function
    | [] -> []::l_aux
    | h::t as l -> aux (l::l_aux) t
  in
  aux [] l;;
suffixes [1,2,3,4];;


let data = [100;110;105;103;102;100;130;150];
  let rec factor x =
    let first = Random.int x in
    Stream.Cons(first, factor (first + x));;
