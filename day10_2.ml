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


let parse_indicator line = 
  let n = String.length line in
  String.sub line 1 (n-2)

let parse_schematic xs =

  begin
  (* List.iter (fun x -> Printf.printf "%s\n" x;) xs; *)


  xs
  |> List.map parse_indicator
  (* |> List.map (fun x -> begin Printf.printf "sch = %s\n" x; x end;) *)
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  end


let parse_joltage line =
  begin

  (* Printf.printf "jol = %s\n" line; *)

  let n = String.length line in
  String.sub line 1 (n-2)
  |> String.split_on_char ','
  |> List.map int_of_string
  end

let parse_input line =
  let indicator = parse_indicator (List.hd line) in
  let schematics = parse_schematic (List.tl (List.rev (List.tl line))) in
  let joltage = parse_joltage (List.hd (List.rev line)) in
  (indicator, schematics, joltage)

module StringSet = Set.Make(String)

let least_toggle (indicator, schematics, joltage) =
  let to_key xs = String.concat "," (List.map string_of_int xs) in
  
  let queue = Queue.create () in
  let n = List.length joltage in
  let start = List.init n (fun _ -> 0) in
  
  Queue.push (start, 0) queue;
  
  let rec bfs visited =
    if Queue.is_empty queue then 
      failwith ("No solution found for " ^ indicator)
    else
      let (current, steps) = Queue.pop queue in
      let key = to_key current in
      
      if current = joltage then 
        steps
      else if StringSet.mem key visited then
        bfs visited
      else if List.exists2 (>) current joltage then
        bfs visited  (* Prune if any component exceeds target *)
      else
        let visited' = StringSet.add key visited in
        (* Try each schematic *)
        List.iter (fun scheme ->
          let next = List.mapi (fun i val_ ->
            val_ + (List.nth scheme i)
          ) current in
          Queue.push (next, steps + 1) queue
        ) schematics;
        bfs visited'
  in
  
  bfs StringSet.empty

let () =
  read_file "day10_input.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map parse_input
  |> List.map least_toggle
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n"

(*
[1 1 0 0] = a
[1 0 1 0] = b
[0 0 1 1] = c
[0 0 1 0] = d 
[0 1 0 1] = e
[0 0 0 1] = f

[3 4 5 7]

a=3, b=0, c=0, d=5, e=1, f=1

a=0
b=3
c=0
d=2
e=4
f=1


transpose
[
1 1 0 0 0 0 > a + b = 3
1 0 0 0 1 0 > a + e = 4
0 1 1 1 0 0 > b + c + d = 5
0 0 1 0 1 1 > c + e + f = 7
]


[3 4 5 7]

ax + by + cz + dw = 3x + 4y + 5z + 7w
*)