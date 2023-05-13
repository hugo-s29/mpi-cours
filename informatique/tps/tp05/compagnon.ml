(* Module de manipulation de vecteurs *)

(*
(* association d'une couleur à chaque entier *)
let color_table = Hashtbl.create 12
let get_color (i: int) =
  match i with
  | -1 -> Graphics.black
  | 2 -> Graphics.rgb 109 129 98
  | 1 -> Graphics.rgb 105 105 0
  | 0 -> Graphics.rgb 168 27 3
  | 3 -> Graphics.rgb 210 160 4
  | 4 -> Graphics.rgb 217 203 162
  | _ ->
    if Hashtbl.mem color_table i then Hashtbl.find color_table i
    else
      let c = Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255) in
      let () = Hashtbl.add color_table i c in
      c
*)


(* Génération d'un jeu de nb_elem données, contenant nb_cluster groupes, d'"étalement" dist *)

(* Constantes pour graphique *)
let cx = 1000
let cy = 1000
let to_x pt = (pt *. (float_of_int cx)) |> int_of_float
let to_y pt = (pt *. (float_of_int cy)) |> int_of_float

(*
(* Dessine un jeu de données *)
let draw_data (d: data): unit =
  Array.iteri (fun i v ->
      let x = to_x v.(0) in
      let y = to_y v.(1) in
      Graphics.set_color Graphics.black ;
      Graphics.fill_circle x y 3
    ) d

(* Dessine une partition. La partition est représentée par un tableau associant à chaque indice de donnée
   son numéro de groupe *)
let draw_clustering (d: data) (cl: int array): unit =
  Array.iteri (fun i v ->
      let c = get_color cl.(i) in
      let x = to_x v.(0) in
      let y = to_y v.(1) in
      Graphics.set_color c ;
      Graphics.fill_circle x y 3
    ) d

(* Dessine une partition. La partition est représenté pare un tableau associant à chaque indice de donnée
   son numéro de groupe. Les "centres" de chaque groupe sont indiqués au moyen d'un tableau centers. *)
let draw_clustering_w_centers (ce: data) (d: data) (cl: int array): unit =
  Array.iteri (fun i v ->
      let c = get_color cl.(i) in
      let x = to_x v.(0) in
      let y = to_y v.(1) in
      Graphics.set_color c ;
      Graphics.fill_circle x y 3
    ) d;
  Array.iteri (fun i v ->
      let c = get_color i in
      let x = to_x v.(0) in
      let y = to_y v.(1) in
      Graphics.set_color Graphics.black;
      Graphics.fill_rect (x-1) (y-1) 12 12;
      Graphics.set_color c ;
      Graphics.fill_rect x y 10 10;
    ) ce

let () =
  Random.self_init ();
  let t = gen_data 5000 10 0.05 in
  Graphics.open_graph " 1000x1000";
  draw_data t;
  Graphics.loop_at_exit [] (fun _ -> ())
*)
