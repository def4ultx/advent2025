open Printf

#use "shared.ml";;

(* let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines *)

let () =
  read_file "day01_input.txt"
  |> List.fold_left (fun (current, count) (line) ->
    let number = int_of_string (String.sub line 1 (String.length line - 1)) in
    let next_dial = (if line.[0] = 'L' then current - number else current + number) mod 100 in
    let clicked = if next_dial = 0 then 1 else 0 in

    (next_dial, count + clicked)
    ) (50, 0)
  |> fun (x,y) -> Printf.printf "%d,%d\n" x y

(* The dial starts by pointing at 50.
The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
The dial is rotated L30 to point at 52.
The dial is rotated R48 to point at 0. **
The dial is rotated L5 to point at 95.
The dial is rotated R60 to point at 55; during this rotation, it points at 0 once.
The dial is rotated L55 to point at 0. **
The dial is rotated L1 to point at 99.
The dial is rotated L99 to point at 0. **
The dial is rotated R14 to point at 14.
The dial is rotated L82 to point at 32; during this rotation, it points at 0 once. *)
