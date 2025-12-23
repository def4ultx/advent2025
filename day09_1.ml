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


let create_rectangles xs =
  List.mapi (fun i (x1,y1) -> 
    List.mapi (fun j (x2,y2) ->
      if i < j && i <> j then
        let dx = abs (x2-x1) + 1 in
        let dy = abs (y2-y1) + 1 in
        Some (dx*dy)
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x)

let input =
  read_file "day09_input.txt"
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.map (fun x -> (List.nth x 0, List.nth x 1))
  |> create_rectangles
  |> List.sort compare
  |> List.rev
  |> List.hd
  |> printf "Total sum: %d\n"
