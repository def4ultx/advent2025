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

let max a b = if a > b then a else b

let rec max_battery m xs =
  match xs with
  | [] -> m
  | x :: t -> max (m*10+x) (max_battery (max m x) t)
    

let turn_on_battery line =
  String.to_seq line
  |> List.of_seq
  |> List.map Char.code
  |> List.map ((+) (-48))
  (* |> List.map (fun x -> 
    begin
    print_int x;
    print_endline "";
    x
    end) *)
  |> max_battery 0
  (* |> (fun x -> 
    begin
    print_int x;
    print_endline "";
    x
    end) *)


let () =
  read_file "day03_input.txt"
  |> List.map turn_on_battery
  |> List.fold_left (+) 0
  |> printf "Total sum of lengths: %d\n"