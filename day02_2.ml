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

    Printf.printf "calc %d -> %d\n" range_from range_to;

    let counter = ref 0 in
    for num = range_from to range_to do
      let rec count_digit n = if n = 0 then 0 else 1 + count_digit(n/10) in
      let rec int_pow base exp = if exp = 0 then 1 else base * (int_pow base (exp-1)) in
      let digit = count_digit num in

      let rec split n m = 
        if n = 0 then []
        else 
          let rem = n mod m in
          rem :: split (n / m) m in

      let all_same = function
        | [] -> true
        | h :: t -> List.for_all ((=) h) t in

      (* List.for_all *)

      let invalid = List.init (digit / 2) (fun i -> i + 1)
      (* |> List.tl *)
      |> List.filter (fun x -> digit mod x = 0)
      |> List.map (fun x -> split num (int_pow 10 x))
      (* |> List.exists all_same in *)
      |> List.exists (fun xs -> List.for_all ((=) (List.hd xs)) xs) in

      if invalid then
        begin
          Printf.printf "found number: %d\n" num;
          counter := !counter + num
        end
    done;

    !counter
    

(*
4174379265 - 4174378023
*)

let () =
  read_file "day02_input.txt"
  |> List.hd
  |> String.split_on_char ','
  |> List.map sum_invalid
  |> List.fold_left (+) 0
  |> printf "Total sum of lengths: %d\n"