let sum (x: int list): int =
  let rec aux (x: int list) (s: int): int =
    match x with
    | [] -> s
    | u::q -> aux q (s + u)
  in aux x 0

let pairs (x: int list): int list =
  let rec aux (x: int list) (p: int list): int list =
    match x with
    | a :: q when a mod 2 = 0 -> aux q (a :: p)
    | _ -> p
  in List.rev (aux x [])

type 'a arbreBinaire =
  | Noeud of 'a arbreBinaire * 'a * 'a arbreBinaire
  | Vide

let bt_sum (t: int arbreBinaire) =
  let rec aux (ts: int arbreBinaire list) (s: int): int =
    match ts with
    | t :: q -> begin
        match t with
        | Noeud(g, k, d) -> aux (g :: d :: q) (s + k)
        | Vide -> aux q s
      end
    | [] -> s
  in aux [t] 0

let bt_insert (t: 'a arbreBinaire) (x: int): 'a arbreBinaire =
  let rec rebuild (t: 'a arbreBinaire) = function
    | [] -> t
    | (k, g, true ) :: others -> rebuild (Noeud(g, k, t)) others
    | (k, d, false) :: others -> rebuild (Noeud(t, k, d)) others
  in
  let rec go_down (t: 'a arbreBinaire) (others: ('a * 'a arbreBinaire * bool) list) =
    match t with
    | Vide -> rebuild (Noeud(Vide, x, Vide)) others
    | Noeud(g, k, d) when k <= x -> go_down d ((k,g,true )::others)
    | Noeud(g, k, d)             -> go_down g ((k,d,false)::others)
  in
  go_down t []

let rec print_elems (x: int list): unit =
  match x with
  | e::q -> begin
      print_int e;
      print_char ',';
      print_elems q
    end
  | [] -> ();

let print_elems_rev (l: int list): unit =
  let rec aux (l: int list) (cont: unit -> unit): unit =
    match l with
    | []     -> cont()
    | x :: q -> aux q (fun () -> (print_int x; print_char ','; cont()))
  in aux l (fun x -> ())

let bt_replace (f: 'a -> 'b) (t: 'a arbreBinaire): 'b arbreBinaire =
  let rec aux (t: 'a arbreBinaire) (cont: 'b arbreBinaire -> 'b arbreBinaire) =
    match t with
    | Noeud(g, k, d) -> aux g (fun resg -> aux d (fun resd -> cont (Noeud(resg, f k, resd))))
    | Vide -> cont Vide
  in aux t (fun t -> t)

