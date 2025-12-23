open Printf

let shift_number current amount = 
  let x = if current = 0 || amount mod 100 < current then 0 else 1 in
  let next = (x+amount/100)*100 + (100 + current - amount mod 100) mod 100 in
  Printf.printf "Next: %d\n" next;
  ()



(* 50 L68 -> 82 (1 clicked) = 250 - 68
0 L5 -> 95 (0 clicked) = 100 - 5
55 L155 -> 0 (2 clicked) = 255 - 55 = 200
20 L25 -> 95 (1 clicked) = 120 - 25 = 95 + 100
20 L125 -> 95 (2 clicked) = 220 - 25 = 195 + 100 *)

let _ = shift_number 50 68 
let _ = shift_number 0 5
let _ = shift_number 55 155
let _ = shift_number 20 25
let _ = shift_number 20 125
let _ = shift_number 20 225
let _ = shift_number 20 205

(* Next: 182
Next: 95
Next: 200
Next: 195
Next: 295
Next: 395
Next: 315 => 215 *)