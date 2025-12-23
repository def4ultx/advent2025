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

let parse_input xs =
  let line = String.split_on_char ':' xs in
  let src = List.hd line in
  let dst = List.nth line 1 |> String.split_on_char ' ' |> List.filter ((<>) "") in
  (src, dst)

let create_graph xs =
  let table = Hashtbl.create 0 in
  List.iter (fun (src, dst) -> Hashtbl.replace table src dst) xs;
  table

let count_path graph = 
  begin
  let aux'  target current =
    begin
    let memo = Hashtbl.create 0 in
    let rec aux target current =
      if current = target then 1
      else
        begin
        (* Printf.printf "%s\n" current; *)
        match Hashtbl.find_opt memo current with
          | Some v -> v
          | None ->
            let neighbor = match Hashtbl.find_opt graph current with | Some v -> v | None -> [] in
            let total = neighbor |> List.map (aux target) |> List.fold_left (+) 0 in
            Hashtbl.replace memo current total;
            total
          end in
    aux target current
    end in
  let x0 = aux' "out" "svr" in
  let x1 = aux' "fft" "svr" in
  let x2 = aux' "dac" "fft" in
  let x3 = aux' "out" "dac" in


  let y1 = aux' "dac" "svr" in
  let y2 = aux' "fft" "dac" in
  let y3 = aux' "out" "fft" in


  Printf.printf "%d: %d %d %d - %d %d %d\n" x0 x1 x2 x3 y1 y2 y3;

  x1*x2*x3 + y1*y2*y3
  end


let () =
  read_file "day11_input.txt"
  |> List.map parse_input
  |> create_graph
  |> count_path
  |> printf "Total sum: %d\n"