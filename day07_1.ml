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


let count_split idx matrix =
  (* let m = Array.length matrix in
  let n = Array.length matrix.(0) in *)

  matrix.(1).(idx) <- '|';

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


  let counter = ref 0 in
  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->
      if i <> 0 && matrix.(i-1).(j) = '|' &&  matrix.(i).(j) = '^' then
        counter := !counter + 1;
    ) matrix.(i);
  ) matrix;
  !counter

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
  |> count_split idx
  |> printf "Total sum: %d\n"