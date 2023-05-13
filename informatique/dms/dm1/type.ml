type 'c btree =
  | N of 'c * 'c btree * 'c btree (* arbre contenant un noeud *)
  | V                             (* arbre vide *)

type 'a ncontent = {
  hauteur : int;                (* hauteur du noeud courant *)
  elem    : 'a ;                (* élément du noeud courant *)
}

type 'a set = 'a ncontent btree

let ex  =
  N({hauteur = 2; elem = 6},
    N({hauteur = 1; elem = 3},
      N({hauteur = 0; elem = 1}, V, V),
      N({hauteur = 0; elem = 4}, V, V)
     ),
    N({hauteur = 0; elem = 8}, V, V)
   )

type 'a cmp = 'a -> 'a -> int

let pp_set (pp_elem: 'a -> unit) (s: 'a set) : unit =
  let rec aux (s: 'a set) (b: bool): bool =
    match s with
    | V -> b
    | N(x, g, d) ->
      begin
        let b' = aux g b in
        if b' then (print_string ", ");
        pp_elem x.elem;
        let _ = aux d true in
        true
      end
  in
  print_string "{";
  let _ = aux s false in
  print_string "}"
