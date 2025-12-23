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


let create_rectangles xs =
  List.mapi (fun i (x1,y1) -> 
    List.mapi (fun j (x2,y2) ->
      if i < j && i <> j then
        let dx = abs (x2-x1) + 1 in
        let dy = abs (y2-y1) + 1 in
        Some (dx*dy, ((x1,y1), (x2,y2)))
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x)

let create_edges input =
  let max_point (x1, y1) (x2, y2) = if y1 > y2 then (x1, y1) else (x2, y2) in
  let min_point (x1, y1) (x2, y2) = if y1 < y2 then (x1, y1) else (x2, y2) in

  let rec aux xs = match xs with
  | [] -> []
  | [_] -> []
  | x :: y :: t ->
    let x' = max_point x y in
    let y' = min_point x y in
     (x', y') :: aux (y :: t) in
  aux (input @ [List.hd input])


(* let find_align_pair xs =
  List.mapi (fun i (x1,y1) -> 
    List.mapi (fun j (x2,y2) ->
      if i < j && i <> j then
        if x2 = x1 || y2 = y1 then Some ((x1,y1), (x2,y2)) else None
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x) *)

let points = 
  read_file "day09_input_example.txt"
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.map (fun x -> (List.nth x 0, List.nth x 1))

let edges = create_edges points
|> List.filter (fun ((x1, y1),(x2, y2)) -> y1 <> y2)


(* let () =
  edges
  |> List.iter (fun ((x1,y1),(x2,y2)) -> Printf.printf "(%d,%d) - (%d,%d)\n" x1 y1 x2 y2) *)

(* let print_matrix_to_file matrix =
begin
  Printf.printf "check out file\n";
  let file = open_out ("day09.txt") in
  Array.iteri (fun i _ ->
    Array.iteri (fun j _ ->
      let c = if matrix.(i).(j) = '.' then " " else "#" in
      Printf.fprintf file "%s" c
    ) matrix.(i);
    Printf.fprintf file "\n";
  ) matrix;
  close_out file;
end *)

(* let matrix = Array.make_matrix 100000 100000 '.'

let () = points
  |> List.iter (fun (x, y) -> matrix.(y).(x) <- 'O')

let () =
  points
  |> find_align_pair
  |> List.iter (fun ((x1,y1), (x2,y2)) ->
    if x1 = x2 then
      for y = min y1 y2 to max y1 y2 do
        matrix.(y).(x1) <- '#'
      done
    else 
      for x = min x1 x2 to max x1 x2 do
        matrix.(y1).(x) <- '#'
      done
  )

let () = print_matrix_to_file matrix *)

(* let () = 
  Array.iter (fun row ->
    Array.iter (fun c -> Printf.printf "%c" c) row;
    Printf.printf "\n"
  ) matrix *)

(* let paint_matrix matrix =  *)

(* let inside_check matrix (x, y) =
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in

  let rec ray_case_left x y =
    if x < 0 then false
    else if matrix.(y).(x) = '#' then true
    else ray_case_left (x-1) y in

  let rec ray_case_right x y =
    if x >= width then false
    else if matrix.(y).(x) = '#' then true
    else ray_case_right (x+1) y in

  let rec ray_case_up x y =
    if y < 0 then false
    else if matrix.(y).(x) = '#' then true
    else ray_case_up x (y-1) in

  let rec ray_case_down x y =
    if y >= height then false
    else if matrix.(y).(x) = '#' then true
    else ray_case_down x (y+1) in

  ray_case_left x y &&
  ray_case_right x y &&
  ray_case_up x y &&
  ray_case_down x y *)

let ray_cast_check edges ps =
begin
  ps
  |> List.map (fun (x, y) ->
    (* Printf.printf "visit %d,%d\n" x y; *)
    let y' = float_of_int y +. 0.1 in
    let x' = float_of_int x in
    edges
    |> List.map (fun ((x2, y2), (x1, y1)) ->
      (* if y' < y1 || y' > y2 then false
      else if x >= max x1 x2 then false
      else 
        let m_red = if x1 <> x2 then ((y2 -. y1) /. (x2 -. x1)) else max_float in
        let m_blue = if x1 <> x2 then ((y' -. y1) /. (x -. x1)) else max_float in
        if m_blue >= m_red then true else false *)

      let (x1', y1') = (float_of_int x1, float_of_int y1) in
      let (x2', y2') = (float_of_int x2, float_of_int y2) in
      let c1 = (y' < y1') <> (y' < y2') in
      (* let t = if y2 = y2 then 1 else ((y-y1) / (y2-y1)) in *)
      let c2 = x' < x1' +. ( ((y'-.y1') /. (y2'-.y1')) *. (x2'-.x1') ) in
      (* let c2 = x <= x1 + ( t * (x2-x1) ) in *)
      (* Printf.printf "edge: (%d,%d) (%d,%d) %b %b -> %b\n" x1 y1 x2 y2 c1 c2 (c1 && c2); *)
      c1 && c2
    )
    |> List.filter ((=) true)
    |> List.length
  )
  |> List.for_all (fun x -> (x mod 2) <> 0)
end


let input =
  points
  |> create_rectangles
  |> List.sort (fun (v1, _) (v2, _) -> compare v1 v2)
  |> List.rev
  |> List.find (fun (area, ((x1,y1), (x2,y2))) ->
    (* Printf.printf "checking (%d,%d),(%d,%d) -> %d\n" x1 y1 x2 y2 area; *)
    (* let p1 = (x1, y1) in *)
    let p2 = (x1, y2) in
    let p3 = (x2, y1) in
    (* let p4 = (x2, y2) in *)
    (* let ps = [p1; p2; p3; p4] in *)
    let ps = [p2; p3] in

    (* let ok = List.for_all (fun (x, y) ->
      (* ray cast this points first and use hash for faster checking *)
      (* Not working well due to data
      List.exists (fun (xx, yy) -> x = xx && y = yy) points *)
      (* condition to check
      1. have point
      2. or inside by casting out 4 direction *)

      (* inside_check matrix (x, y) *)
      true
    ) ps in *)

    let ok = ray_cast_check edges ps in

    if ok then
      Printf.printf "ok (%d,%d),(%d,%d)\n" x1 y1 x2 y2;

    ok
  )
  |> fun (area, _) -> area
  |> printf "Total sum: %d\n"
