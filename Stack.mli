module Stack :
  sig
    type a
    type t = a list
    val empty : 'a list
    val isEmpty : 'a list -> bool
    val cons : 'a -> 'a list -> 'a list
    val head : 'a list -> 'a
    val tail : 'a list -> 'a list
  end
