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

let parse_input xs =
  let line = String.split_on_char ':' xs in
  let src = List.hd line in
  let dst = List.nth line 1 |> String.split_on_char ' ' |> List.filter ((<>) "") in
  (src, dst)

let create_graph xs =
  let table = Hashtbl.create 0 in
  List.iter (fun (src, dst) -> Hashtbl.replace table src dst) xs;
  table

let count_path graph = 
  let memo = Hashtbl.create 0 in
  let rec aux current =
    if current = "out" then 1
    else
      match Hashtbl.find_opt memo current with
        | Some v -> v
        | None ->
          let neighbor = Hashtbl.find graph current in
          let total = neighbor |> List.map aux |> List.fold_left (+) 0 in
          Hashtbl.replace memo current total;
          total
      in
  aux "you"


let () =
  read_file "day11_input.txt"
  |> List.map parse_input
  |> create_graph
  |> count_path
  |> printf "Total sum: %d\n"