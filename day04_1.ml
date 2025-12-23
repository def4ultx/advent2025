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

let copy src dst =
  begin
  Array.iteri (fun i xi ->
    Array.iteri (fun j x -> dst.(i).(j) <- x) xi
  ) src;
  dst
  end

let count_valid input =
  let m = Array.length input in
  let n = Array.length input.(0) in
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  let counter = ref 0 in

  let input' = copy input (Array.make_matrix m n '.') in

  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->

      let c = directions
      |> List.map (fun (di, dj) ->
        let r = i+di in
        let c = j+dj in


        if r >= 0 && r < m && c >= 0 && c < n && input.(r).(c) = '@' then 1 else 0
      )
      |> List.fold_left (+) 0 in
      if c < 4 && input.(i).(j) = '@'  then
        begin
        input'.(i).(j) <- 'x';
        counter := !counter + 1;
        end;

      Printf.printf "%c" input'.(i).(j);
    ) input.(i);



    print_endline "";
  ) input;

  !counter

let () =
  read_file "day04_input.txt"
  |> List.map String.to_seq
  |> List.map Array.of_seq
  |> Array.of_list
  |> count_valid
  |> printf "Total sum: %d\n"