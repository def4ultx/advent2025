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

let calculate line =
  let parts = String.split_on_char ':' line in
  let area =
    List.hd parts
    |> String.split_on_char 'x'
    |> (fun xs -> (List.nth xs 0), (List.nth xs 1))
    |> (fun (a,b) -> int_of_string a * int_of_string b) in
  let expected = 
    List.tl parts 
    |> List.hd 
    |> String.split_on_char ' ' 
    |> List.filter ((<>) "") 
    |> List.map int_of_string
    |> List.fold_left ( + ) 0 in
  (area, expected*9)

let () =
  read_file "day12_input.txt"
  |> List.map calculate
  |> List.map (fun (a,b) -> begin Printf.printf "a=%d,b=%d\n" a b; (a,b) end)
  |> List.map (fun (a,b) -> a >= b)
  |> List.filter ((=) true)
  |> List.length
  |> printf "Total sum: %d\n"