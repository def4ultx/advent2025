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

let rec transpose matrix =
  if matrix = [] || List.hd matrix = [] then []
  else
    (List.map List.hd matrix) :: transpose (List.map List.tl matrix)

let calculate (c, n) xs =
  begin
  let op = if c = '*' then ( * ) else ( + ) in
  let ac = if c = '*' then 1 else 0 in

  Printf.printf "Calculate op=%c size=%d\n" c n;
  List.iter (fun x -> Printf.printf "value %s\n" x) xs;

  List.init n (fun i -> i)
  |> List.map (fun i -> xs |> List.map (fun x -> if i < String.length x then x.[i] else ' '))
  |> List.map (fun str -> str |> List.filter (fun c -> c <> ' ') |> List.to_seq |> String.of_seq)
  |> List.map (fun x -> begin Printf.printf "str value = %s\n" x; x end)
  |> List.map int_of_string
  |> List.fold_left op ac
  end

(* let () = read_file "day06_input_example.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter (fun x -> x <> ""))
  |> transpose
  |> List.map List.rev
  |> List.map calculate
  |> List.map (fun x -> begin Printf.printf "final value = %d\n" x; x end)
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n" *)


let rec to_header n chars =
  match chars with
  | [] -> []
  | x :: t when x = ' ' -> to_header (n+1) t
  | x :: t -> (x, n) :: to_header 0 t

let header = read_file "day06_input.txt"
  |> List.rev
  |> List.hd
  |> String.to_seq
  |> List.of_seq
  |> List.rev
  |> to_header 1
  |> List.rev
  (* |> List.iter (fun (o,n) -> Printf.printf "header: %c-%d\n" o n) *)

let rec parse_number header str =
  match header with
  | [] -> []
  | (_, n) :: t ->
    (* begin
      Printf.printf "str=%s\n" str;
      Printf.printf "hd=%s\n" (String.sub str 0 n);
      Printf.printf "tl=%s\n" (String.sub str (n+1) (String.length str - n - 1)); *)
    (String.sub str 0 n) :: parse_number t (String.sub str (n+1) (String.length str - n - 1))
    (* end *)

let input = read_file "day06_input.txt"
  |> List.rev
  |> List.tl
  |> List.rev
  |> List.map (fun s -> String.cat s " ")
  |> List.map (parse_number header)
  |> transpose
  |> List.mapi (fun i xs -> calculate (List.nth header i) xs)
  |> List.map (fun x -> begin Printf.printf "final value = %d\n" x; x end)
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n"