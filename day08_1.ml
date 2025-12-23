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


let euclidean_distance xs =
  List.mapi (fun i (x1,y1,z1) -> 
    List.mapi (fun j (x2,y2,z2) ->
      if i < j && i <> j then
        let dx = float_of_int (x2-x1) in
        let dy = float_of_int (y2-y1) in
        let dz = float_of_int (z2-z1) in
        let d = sqrt (dx*.dx +. dy*.dy +. dz*.dz) in
        Some (i, j, d)
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x)

let union_find size xs = 
  let dsu = Array.init size (fun x -> x) in
  let counter = ref 0 in
  let first_pair = ref (0, 0) in

  let rec dsu_find idx =
    if dsu.(idx) == idx then idx
    else dsu_find dsu.(idx) in

  let rec dsu_union i j =
    begin
    let parent_i = dsu_find i in
    let parent_j = dsu_find j in 

    if parent_i <> parent_j then
      counter := !counter + 1;

    if size - !counter = 1 then
      (* Printf.printf "All connected at union %d %d\n" i j; *)
      first_pair := (i, j);

    dsu.(parent_i) <- parent_j
    end in

  List.iter (fun (i, j, _) -> dsu_union i j) xs;

  let group_by lst =
    let table = Hashtbl.create (List.length lst) in
    List.iter (fun x ->
      let count = match Hashtbl.find_opt table x with
        | Some n -> n+1
        | None -> 1
      in
      Hashtbl.replace table x count
    ) lst;
    table in

  (* let () = Array.to_list dsu
  |> List.iteri (fun i v -> Printf.printf "dsu p=%d i=%d,v=%d\n" (dsu_find i) i v) in *)
  
  Array.to_list dsu
  |> List.mapi (fun i _ -> dsu_find i)
  |> group_by
  |> Hashtbl.to_seq
  |> List.of_seq
  (* |> List.map (fun (k, v) -> begin Printf.printf "%d %d\n" k v ; (k, v) end;) *)
  |> List.map (fun (k, v) -> v)
  |> List.filter ((<) 1)
  |> List.sort compare
  |> List.rev
  |> List.take 3
  (* |> List.map (fun x -> begin Printf.printf "result = %d\n" x ; x end;) *)
  |> List.fold_left ( * ) 1

let size = read_file "day08_input_example.txt" |> List.length

let () =
  read_file "day08_input_example.txt"
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.map (fun x -> (List.nth x 0, List.nth x 1, List.nth x 2))
  |> euclidean_distance
  |> List.sort (fun (_, _, d1) (_, _, d2) -> compare d1 d2)
  |> List.take 1000
  (* |> List.map (fun (i, j, d) -> begin Printf.printf "%d,%d %f\n" i j d; (i, j, d) end; ) *)
  |> union_find size
  |> printf "Total sum: %d\n"