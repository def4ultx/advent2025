open Printf

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines


let mark_split matrix =
  (* let m = Array.length matrix in
  let n = Array.length matrix.(0) in *)

  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->
      begin
      if i <> 0 && matrix.(i-1).(j) = '|' then
        let c = matrix.(i).(j) in
        if c = '.' then
          matrix.(i).(j) <- '|'
        else if c = '^' then
          begin
          matrix.(i).(j+1) <- '|';
          matrix.(i).(j-1) <- '|';
          end
        end;
    ) matrix.(i);
  ) matrix;


  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->
      Printf.printf "%c" matrix.(i).(j);
    ) matrix.(i);
    print_endline "";
  ) matrix;

  matrix


(* if i < 0 || i > m || j < 0 || j > n then 0
if memo <> -1 then memo


if c = '.' then 0
if c = '|' and j-1 = '^'
    1+ count (i-1, j-1)
if c = '|' and j+1 = '^'
    1+ count (i-1, j+1)
count (i-1,j) *)
(* let count_split matrix =
  let m = Array.length matrix in
  let n = Array.length matrix.(0) in
  let memo = Array.make_matrix m n (-1) in
  (*input*)
  (* memo.(1).(70) <- 1;  *)
  (*example*)
  memo.(1).(8) <- 1;
  let rec aux i j =
    if i < 0 || i > m || j < 0 || j > n then 0
    else if memo.(i).(j) <> -1 then memo.(i).(j)
    else if matrix.(i).(j) = '.' then 0
    else
      begin
        let c = matrix.(i).(j) in
        let left = if c = '|' && matrix.(i).(j-1) = '^' then 1+aux (i-1) (j-1) else 0 in
        let right = if c = '|' && matrix.(i).(j+1) = '^' then 1+aux (i-1) (j+1) else 0 in
        let top = if c = '|' then aux (i-1) (j) else 0 in
        let total = left+right+top in

        memo.(i).(j) <- total;
        total
      end in
  
  matrix.(m-1)
  |> Array.mapi (fun j _ -> aux (m-1) j)
  |> Array.to_list
  |> List.fold_left (+) 0 *)
  

let count_split idx matrix =
  let m = Array.length matrix in
  let n = Array.length matrix.(0) in
  let memo = Array.make_matrix m n (-1) in
  let rec aux i j =
      (* Printf.printf "Visiting (%d, %d)\n" i j; *)
    
    if i < 0 || i > m || j < 0 || j > n then 0
    else if i = m then 1
    else if memo.(i).(j) <> -1 then memo.(i).(j)
    else
      let total = if matrix.(i).(j) = '.' then aux (i+1) (j)
      else if matrix.(i).(j) = '^' then (aux (i) (j-1)) + (aux (i) (j+1))
      else 0 in

      memo.(i).(j) <- total;

      total in
  aux 1 idx

let to_matrix xs = 
  let m = Array.length xs in
  let n = Array.length xs.(0) in
  let matrix = Array.make_matrix m n '.' in

  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->
      matrix.(i).(j) <- xs.(i).(j);
    ) matrix.(i);
  ) matrix;

  matrix

let rec find_idx x n a =
  if a.(n) = x then n
  else find_idx x (n+1) a

let idx = read_file "day07_input.txt"
  |> List.hd
  |> String.to_seq
  |> Array.of_seq
  |> find_idx 'S' 0

let () =
  read_file "day07_input.txt"
  |> List.map String.to_seq
  |> List.map Array.of_seq
  |> Array.of_list
  |> to_matrix
  (* |> mark_split *)
  |> count_split idx
  |> printf "Total sum: %d\n"


