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

let next_number (ch, current, amount) =
  begin
    let next = if ch = 'L' then


        (*
        edge case

        50 L68 -> 82 (1 clicked) = 250 - 68
        0 L5 -> 95 (0 clicked) = 100 - 5
        55 L55 -> 0 (1 clicked) = 155 - 55 = 100
        55 L155 -> 0 (2 clicked) = 255 - 55 = 200
        20 L25 -> 95 (1 clicked) = 120 - 25 = 95 + 100
        20 L125 -> 95 (2 clicked) = 220 - 25 = 195 + 100

        current + 100 + (x+amount/100)*100 - amount mod 100

        if current <= amount then
          100 + ((x+amount/100)*100) - amount mod 100
        
        *)


      (* let shift = 

        if current <= amount && current <> 0 then
          (2 + amount / 100) * 100
        else if current = 0 then
          (amount / 100) * 100
        else 0 in

         *)

      if current < amount then
        (* let x = if current <> 0 then 1 else 0 in
        let shift = ((x+(amount/100))*100)+100+current in
        let new_amount = amount mod 100 in
        Printf.printf "XX => Current: %d, Shift: %d:%d, Amount: %d\n\n" current x shift new_amount;

        shift - new_amount *)
        let x = if current = 0 || amount mod 100 < current then 0 else 1 in
        let next = (x+amount/100)*100 + (100 + current - amount mod 100) mod 100 in
        next
      else if current = amount then
        100
      else
        current - amount
    else
      current + amount in
    let click = abs(next / 100) in
    let remaining = next mod 100 in
    (remaining, click)
  end

(* let next_number (ch, current, amount) =
  begin
    let next = if ch = 'L' then current - amount else current + amount in
    let click = abs(next / 100) in
    let remaining = next mod 100 in

    if ch = 'L' && remaining = 0 then
      (remaining, click+1)
    else if ch = 'L' && remaining < 0 && current <> 0 then
      (remaining + 100, click+1)  
    else if remaining < 0 then
      (remaining + 100, click)  
    else
      (remaining, click)
  end *)

(*
edge case

0 L5 -> 95 +0 (click = 0)
55 L55 -> 0 +1 (click = 0 but equal to 0, so +1)
20 L25 -> 95 +1 (click = 0 but less than 0, so +1)
20 L125 -> 95 +2 (click = 1 but less than 0, so +1)

if L && remaining <= 0 && current <> 0
*)

    (* if remaining < 0 then 
      if current = 0 then (remaining + 100, click)
      else (remaining+100, click + 1)
    else if next = 0 then
      (remaining, click + 1)
    else  (remaining, click) *)
    (* if next = 0 || next = 100 then
      (next, 1)
    else
      begin
        let count = ref 0 in
        let i = ref next in
        while !i < 0 do
          i := !i + 100;
          count := !count + 1;
        done;
        while !i >= 100 do
          i := !i - 100;
          count := !count + 1;
        done;
        (* let edge = if next < 0 && next > -100 then 1 else 0 in *)
        (* if !i = 0 then count := !count + 1; *)

        if current = 0 && next < 0 then
          count := !count - 1;

        (!i, !count)
      end *)


let () =
  read_file "day01_input.txt"
  |> List.fold_left (fun (current, count) (line) ->
    let number = int_of_string (String.sub line 1 (String.length line - 1)) in

    (* let clicked = (abs number) / 100 in *)
    let (next_dial, clicked) = next_number (line.[0], current, number) in
    let new_count = if next_dial = 0 then 1 else 0 in

    Printf.printf "Current: %d, Number: %c:%d, Next: %d, Count: %d, Click: %d\n" current line.[0] number next_dial new_count clicked;

    (next_dial, count + clicked)
    ) (50, 0)
  |> fun (x,y) -> Printf.printf "%d,%d\n" x y

(*
6932

The dial starts by pointing at 50.
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


(* The dial is rotated L68 to point at 82.
The dial is rotated L30 to point at 52.
The dial is rotated R48 to point at 0.
The dial is rotated L5 to point at 95.
The dial is rotated R60 to point at 55.
The dial is rotated L55 to point at 0.
The dial is rotated L1 to point at 99.
The dial is rotated L99 to point at 0.
The dial is rotated R14 to point at 14.
The dial is rotated L82 to point at 32. *)


