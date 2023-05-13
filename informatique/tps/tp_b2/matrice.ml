type matrice = int array array

let remove_last (l: 'a list): 'a list =
  match List.rev l with
  | [] -> []
  | x :: q -> List.rev q


let import (filename: string): matrice =
  let channel = open_in filename in
  let nbl = input_line channel |> int_of_string in
  let nbc = input_line channel |> int_of_string in
  let mat = Array.init nbc (fun _ ->
    input_line channel
    |> String.split_on_char ' '
    |> remove_last
    |> List.map int_of_string
    |> Array.of_list
  ) in
  close_in channel;
  mat

let printed (x: string): string = print_string x; x

let export (filename: string) (mat: matrice): unit =
  let channel = open_out filename in
  let n, m = Array.length mat, Array.length mat.(0) in
  output_string channel (string_of_int m ^ "\n" ^ string_of_int n ^ "");
  Array.iter (fun line ->
    Array.map string_of_int line
    |> Array.map (fun x -> x ^ " ")
    |> Array.fold_left (^) "\n"
    |> output_string channel
  ) mat;
  close_out channel

let () =
  let argc = Array.length Sys.argv in
  if (argc <> 3) then
    Printf.printf "usage : ./exec filename_src filename_dst"
  else
    let m = import Sys.argv.(1) in
    let m2 = Array.map (fun l -> Array.map (fun x -> x+1) l) m in
    export Sys.argv.(2) m2
