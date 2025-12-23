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

(* let rec max_battery m c xs =
  match xs with
  | [] -> m
  | x :: t -> 
    begin
    max (m*10+x) (max_battery (max m x) t) *)


(* let rec find_max m c xs =
  if c = 0 then m
  else
    match xs with
    | [] -> 0
    | x :: t ->
      let x1 = find_max m c t in
      let x2 = find_max (m*10+x) (c-1) t in
      max x1 x2 *)

let rec count_digit n = if n = 0 then 0 else 1 + count_digit(n/10)
let rec int_pow base exp = if exp = 0 then 1 else base * (int_pow base (exp-1))

(* let rec find_max c xs =
  if c = 0 then 0
  else
    match xs with
    | [] -> 0
    | x :: t ->
      let x1 = find_max c t in

      let t = find_max (c-1) t in

      let d = count_digit t in
      let x2 = x * (int_pow 10 d) + t in

      max x1 x2 *)

(* working *)

(* let max_battery xs =
    let l = List.length xs in
    let memo = Array.make_matrix l 12 (-1) in

    let rec find_max i c =
      if i >= l then 0
      else if c < 0 then 0
      else if memo.(i).(c) <> -1 then memo.(i).(c)
      else
        begin
          let x1 = find_max (i+1) c in
          let t = find_max (i+1) (c-1) in
          let d = count_digit t in
          let x2 = (List.nth xs i) * (int_pow 10 d) + t in

          (* (11-c) should work *)

          let m = max x1 x2 in
          memo.(i).(c) <- m;
          m
        end in
    find_max 0 11 *)



let max_battery xs =
  begin
  let c = 12 in
  let l = List.length xs in
  let xs' = List.rev xs in
  let memo = Array.make_matrix (c) (l) (0) in

  memo.(0).(0) <- List.hd xs';
  List.iteri (fun i x -> 
    if i > 0 then
      memo.(0).(i) <- max memo.(0).(i-1) x
  ) xs';

  Array.iteri (fun i _ ->
    Array.iteri (fun j n ->
      if i <> 0 && j <> 0 && i <= j then
        let x1 = memo.(i).(j-1) in
        let t = memo.(i-1).(j-1) in
        let x2 = t + ((List.nth xs' j) * (int_pow 10 (count_digit t))) in

        let m = max x1 x2 in
        memo.(i).(j) <- m;

    ) (Array.of_list xs');
  ) (Array.make c 0);
  

  memo.(c-1).(l-1)
  end


(* module IntStack = Stack.Make(Int) *)

let compute stack =
  Stack.to_seq stack
  |> List.of_seq
  |> List.rev
  |> List.fold_left (fun acc curr -> acc*10 + curr) 0

let remove_k_digits k line =

  Printf.printf "start %s\n" line;
  let l = String.length line in
  let stack = Stack.create () in
  let input = String.to_seq line 
  |> List.of_seq 
  |> List.map Char.code 
  |> List.map ((+) (-48)) in

  Array.of_list input
  |> Array.iteri (fun i n ->
    Printf.printf "i:%d n:%d with stack %d\n" i n (compute stack);

    (*monotonic stack
    ((Stack.length stack) + l-i) ==> After this there will be more than k character
    *)
      while not (Stack.is_empty stack) && n > (Stack.top stack) && ((Stack.length stack) + l-i) > k do
        Stack.pop stack;
      done;

      if Stack.length stack < k then
        Stack.push n stack;

    Printf.printf "result stack %d \n" (compute stack);
  );

  (* Stack.iter (fun x -> Printf.printf "%d" x) stack;
  Printf.printf "%d" (Stack.top stack);
  print_endline ">>"; *)

  Stack.to_seq stack
  |> List.of_seq
  |> List.rev
  |> List.fold_left (fun acc curr -> acc*10 + curr) 0
  (* Stack.fold (fun acc curr -> acc*10 + curr) 1 stack *)
  
  (* let _ = List.iter (fun cur ->
    begin
    if (Stack.is_empty stack) && cur > (Stack.top stack) then
      Stack.pop stack;
    
    if Stack.length stack < k then
      Stack.push cur stack;

    end
  ) in *)




  (* stack := []

  while len(stack) > 0 && current > stack.top {
    stack.pop
  } *)





  (* |> List.fold_left (fun (s, k) x ->
    let rec aux s k x =
      if k > 0 then
        match Stack.top_opt s with
        | Some top when top < x ->
          Stack.pop s;
          aux s (k-1) x
        | _ -> s
      else s
    in
    let s' = aux s k x in
    Stack.push x s';
    (s', k)
  ) (stack, 12) in *)



let turn_on_battery line =
  String.to_seq line 
  |> List.of_seq 
  |> List.map Char.code 
  |> List.map ((+) (-48)) 
  (* |> List.map (fun x -> 
    begin
    print_int x;
    x
    end)
  |> (fun x -> begin print_endline ""; x end) *)
  |> max_battery

let () =
  read_file "day03_input.txt"
  |> List.map (remove_k_digits 12)
  |> List.map (fun x -> Printf.printf "result = %d\n" x; x)
  |> List.fold_left (+) 0
  |> printf "Total sum of lengths: %d\n"



(* # # Old version with simpler logic but less efficient (O(K * N)).
# def max_power_general(line: List[int], nb_digits: int = 12) -> int:
#     digits: List[int] = []
#     position = 0
#     for i in range(nb_digits - 1, -1, -1):
#         if i > 0:
#             max_in_line = max(line[position:-i])
#         else:
#             max_in_line = max(line[position:])
#         digits.append(max_in_line)
#         position = line.index(max_in_line, position) + 1
#     return int("".join(map(str, digits))) *)