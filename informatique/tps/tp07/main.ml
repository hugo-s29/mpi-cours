(* À DÉFINIR *)
type graphe = int list array

let g1: graphe =
	[|
		[1;2;3];                (* 0 ou a*)
		[4];                    (* 1 ou b*)
		[3;4];                  (* 2 ou c*)
		[5];                    (* 3 ou d*)
		[5];                    (* 4 ou e*)
		[]                      (* 5 ou f*)
	|]


let g2: graphe =
	[|
		[1];                    (* 0 ou a*)
		[2;4];                  (* 1 ou b*)
		[3];                    (* 2 ou c*)
		[];                     (* 3 ou d*)
		[0];                    (* 4 ou e*)
		[3]                     (* 5 ou f*)
	|]

let g3: graphe =
	[|
		[1;4];                  (* 0 ou a*)
		[5;6];                  (* 1 ou b*)
		[1];                    (* 2 ou c*)
		[0];                    (* 3 ou d*)
		[3;5];                  (* 4 ou e*)
		[6];                    (* 5 ou f*)
		[2;7];                  (* 6 ou g*)
		[2]                     (* 7 ou h*)
	|]

let g4: graphe =
	[|
		[5];                    (* 0 ou a*)
		[0;2];                  (* 1 ou b*)
		[3];                    (* 2 ou c*)
		[1];                    (* 3 ou d*)
		[0;3];                  (* 4 ou e*)
		[4]                     (* 5 ou f*)
	|]

let gpiege =
	[|
		[2;3];                  (* 0 ou a*)
		[0];                    (* 1 ou b*)
		[5];                    (* 2 ou c*)
		[2;4];                  (* 3 ou d*)
		[];                     (* 4 ou e*)
		[]                      (* 5 ou f*)
	|]


let g_test =
  [|
    [];                         (* 0 *)
    [0; 2];                     (* 1 *)
    [1; 3];                     (* 2 *)
    [4];                        (* 3 *)
    [2];                        (* 4 *)
    [3];                        (* 5 *)
    [4; 7];                     (* 6 *)
    [5; 6];                     (* 7 *)
  |]

(*let transpose (g: graphe): graphe =
  Array.mapi
  (fun i s -> List.mapi (fun j s' -> (j, s')) g.(i)
  |> List.filter (fun (j, s') -> s' = i)
  |> List.map (fun (j, s') -> j))
  g*)

let transpose (g: graphe): graphe =
  let n = Array.length g in
  let g' = Array.make n [] in
  for i = 0 to n - 1 do
    List.iter (fun j -> g'.(j) <- i :: g'.(j)) g.(i)
  done;
  g'

exception Plus_d_entiers
let cree_enumerateur_entiers (n: int): (int ref) * (unit -> unit) =
  let elem = ref 0 in
  let next () =
    if !elem < n then elem := !elem + 1
    else raise Plus_d_entiers
  in (elem, next)


let cree_enumerateur_tab (tab: int array): (int ref) * (unit -> unit) =
  let i = ref 0 in
  let elem = ref tab.(0) in
  let next () =
    i := !i + 1;
    elem := tab.(!i)
  in
  (elem, next)

exception PlusDeRacine
let cree_enumerateur_priorite (prio: int array): (int ref) * (bool array -> unit) =
  let elem = ref prio.(0) in
  let n = Array.length prio in
  let next (suc: bool array) =
    let res = List.fold_left
    (fun a b -> 
      match a with
      | None -> Some(b)
      | Some(x) ->
          if not suc.(x) && prio.(x) > prio.(b) then
            Some(x)
          else
            Some(b))
    None (List.init n (fun i -> i))
    in
    match res with
    | None -> raise PlusDeRacine
    | Some(x) -> elem := x
  in (elem, next)

