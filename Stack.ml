module Stack = struct
  type 'a stack = Nil | Cons of 'a * 'a stack
  let empty = Nil
  let isEmpty s = (s = Nil)
  let cons x s = Cons (x, s)
  let head = function
    |Nil -> raise (Failure "Stack is vide")
    |Cons (h, t) -> h
  let rec tail = function
      | Nil -> raise (Failure "Stack is vide")
      | Cons (h, Nil) -> h
      | Cons (h, t) -> tail t
end
