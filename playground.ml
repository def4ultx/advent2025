open Printf
(* let sum_invalid range =
for i = 11 to 22 do
  Printf.printf "%d\n" i
done;
()


let () = sum_invalid 10 *)

let number = 1000110001

let rec count_digit n = if n = 0 then 0 else 1 + count_digit(n/10)

let digit = count_digit number
let digits = List.init (digit / 2) (fun i -> i + 1)
|> List.tl
|> List.filter (fun x -> digit mod x = 0)

(* let _ = List.map (fun x ->
  Printf.printf "%d\n" x
) digits *)


let _ = (Char.code ('1') - 48 )
|> Printf.printf "%d\n"