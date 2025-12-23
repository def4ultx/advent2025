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

let rec transpose matrix =
  if matrix = [] || List.hd matrix = [] then []
  else
    (List.map List.hd matrix) :: transpose (List.map List.tl matrix)

let calculate line =
  let op = if List.hd line = "*" then ( * ) else ( + ) in
  let ac = if List.hd line = "*" then 1 else 0 in
  List.tl line
  |> List.map int_of_string
  |> List.fold_left op ac

let () = read_file "day06_input.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (fun x -> x <> ""))
  |> transpose
  |> List.map List.rev
  |> List.map (fun xs -> List.hd xs :: List.rev (List.tl xs))
  |> List.map calculate
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n"
