type 'a btree =
  | Empty
  | Node if 'a * 'a btree * 'a btree

type path = bool list

let next = function
  | b :: q -> 

