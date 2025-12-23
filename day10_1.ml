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
  |> List.map (fun x -> begin Printf.printf "sch = %s\n" x; x end;)
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
  begin
  let queue = Queue.create () in
  (* let visited = StringSet.empty in *)
  let rec aux visited =
    let (current, count) = Queue.pop queue in
    if current = indicator then count
    else
      begin
      let new_visited = StringSet.add current visited in

      (* Printf.printf "aux %s %s %d\n" current indicator count; *)
      let () = schematics
      |> List.iter (fun scheme ->
        let c = current |> String.to_seq |> Array.of_seq in
        let () = scheme |> List.iter (fun i -> if c.(i) = '.' then c.(i) <- '#' else c.(i) <- '.') in
        let next = String.of_seq (Array.to_seq c) in

        if not (StringSet.mem next visited) then
          Queue.push (next, count+1) queue
      ) in
      aux new_visited
      end in

  let start = String.init (String.length indicator) (fun _ -> '.') in
  Printf.printf "least_toggle %s -> %s\n" start indicator;
  Queue.push (start, 0) queue;

  (* schematics
  |> List.iter (fun sch ->

    let c = start |> String.to_seq |> Array.of_seq in
    let () = sch |> List.iter (fun i -> if c.(i) = '.' then c.(i) <- '#' else c.(i) <- '.') in
    let next = String.of_seq (Array.to_seq c) in


    let cc = start |> String.to_seq |> Array.of_seq in
    printf "%s -> %s\n" (String.of_seq (Array.to_seq cc)) next;
    
    sch
    |> List.iter (fun i ->
      Printf.printf "%d\n" i;
    );

    print_endline "---";
  ); *)

  aux StringSet.empty
  end

(* let least_toggle (indicator, schematics, joltage) = 
  let memo = Hashtbl.create (String.length indicator) in
  let rec aux
      Printf.printf "aux %s\n" current;
    if current = indicator then 0
    else if Hashtbl.mem memo current then Hashtbl.find memo current
    else 
      let count = schematics
      |> List.map (fun scheme ->
        let c = indicator |> String.to_seq |> Array.of_seq in
        scheme |> List.iter (fun i -> if c.(i) =  current = 
    begin'.' then c.(i) <- '#' else c.(i) <- '.');
        1 + (aux (String.of_seq (Array.to_seq c)))
      )
      |> List.fold_left min Int.max_int in

      Hashtbl.replace memo current count;
      count
    end in

  let start = String.init (String.length indicator) (fun _ -> '.') in
  Printf.printf "least_toggle %s -> %s\n" start indicator;
  aux start *)

let () =
  read_file "day10_input.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map parse_input
  |> List.map least_toggle
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n"
