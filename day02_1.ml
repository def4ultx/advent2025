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

let sum_invalid range =
    let split = String.split_on_char '-' range in
    let range_from = int_of_string (List.nth split 0) in
    let range_to = int_of_string (List.nth split 1) in

    (* Printf.printf "calc %d -> %d\n" range_from range_to; *)

    let counter = ref 0 in
    for num = range_from to range_to do
      let rec count_digit n = if n = 0 then 0 else 1 + count_digit(n/10) in
      let rec int_pow base exp = if exp = 0 then 1 else base * (int_pow base (exp-1)) in
      let digit = count_digit num in
      if digit mod 2 = 0 then
        let m = digit/2 in
        let b = (int_pow 10 (m)) in
        let left = num mod b in
        let right = num / b in


        (* Printf.printf "check: digit %d, base %d, %d -> %d\n" digit b left right; *)

        if left = right then
          counter := !counter + num
    done;

    !counter
    

let () =
  read_file "day02_input.txt"
  |> List.hd
  |> String.split_on_char ','
  |> List.map sum_invalid
  |> List.fold_left (+) 0
  |> printf "Total sum of lengths: %d\n"