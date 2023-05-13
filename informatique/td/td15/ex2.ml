type 'a consequence = { p: 'a list; c: 'a list }
type 'a sc = 'a consequence list;

let suppose (faits: 'a list) (d: 'a sc): 'a sc =
  List.map (fun {p;c} -> let p = List.filter (fun x -> List.mem x faits) p in {p; c}) d

let rec trouve_premisse_vide (d: 'a sc): 'a consequence option =
  match d with
  | [] -> None
  | {p = []; c} :: q -> Some({p = []; c})
  | {p; c} :: q -> trouve_premisse_vide q
