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

let create_rec xs =
  let rec aux xs1 xs2 =
    if List.length xs1 = 0 then []
    else if List.length xs2 = 0 then aux (List.tl xs1) (List.tl xs1)
    else 
      let (x1, y1) = List.hd xs1 in
      let (x2, y2) = List.hd xs2 in
      let dx = abs (x2-x1) + 1 in
      let dy = abs (y2-y1) + 1 in
      let x1' = float_of_int (max x1 x2) -. 0.5 in
      let y1' = float_of_int (max y1 y2) -. 0.5  in
      let x2' = float_of_int (min x2 x1) +. 0.5 in
      let y2' = float_of_int (min y2 y1) +. 0.5 in
      (dx*dy, ((x1',y1'), (x2',y2'))) :: aux xs1 (List.tl xs2) in
  aux xs (List.tl xs)


let create_rectangles xs =
  List.mapi (fun i (x1,y1) -> 
    List.mapi (fun j (x2,y2) ->
      if i < j && i <> j then
        let dx = abs (x2-x1) + 1 in
        let dy = abs (y2-y1) + 1 in
        let x1' = float_of_int (max x1 x2) -. 0.5 in
        let y1' = float_of_int (max y1 y2) -. 0.5  in
        let x2' = float_of_int (min x2 x1) +. 0.5 in
        let y2' = float_of_int (min y2 y1) +. 0.5 in

        Some (dx*dy, ((x1',y1'), (x2',y2')))
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x)


let min_point (x1, y1) (x2, y2) = if y1 < y2 then (x1, y1) else (x2, y2)
let max_point (x1, y1) (x2, y2) = if y1 >= y2 then (x1, y1) else (x2, y2)

let create_edges input =

  let rec aux xs = match xs with
  | [] -> []
  | [_] -> []
  | x :: y :: t ->
    (x, y)  :: aux (y :: t) in
    (* let x' = min_point x y in
    let y' = max_point x y in
     (x', y') :: aux (y :: t) in *)
  aux (input @ [List.hd input])


let find_align_pair xs =
  List.mapi (fun i (x1,y1) -> 
    List.mapi (fun j (x2,y2) ->
      if i < j && i <> j then
        if x2 = x1 || y2 = y1 then Some ((x1,y1), (x2,y2)) else None
      else None
    ) xs
  ) xs 
  |> List.flatten 
  |> List.filter_map (fun x -> x)
  |> List.map (fun (x, y) -> ((min_point x y), (max_point x y)))

let points = 
  read_file "day09_input.txt"
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.map (fun x -> (List.nth x 0, List.nth x 1))

let edges = create_edges points

(* let edge_compute = find_align_pair points *)
(* |> List.filter (fun ((x1,y1),(x2,y2)) -> y1 <> y2) *)

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

  (* let check_horizontal_line (x, y) (x1, y1) (x2, y2) =
    begin
      let (x1', y1') = (float_of_int x1, float_of_int y1) in
      let (x2', y2') = (float_of_int x2, float_of_int y2) in
      let x' = x in
      let y' = y in 

      if y1 = y2 then
        if (min x1' x2') <= x' && x' < (max x1' x2') && y' < y1' then true else false 
      else
        failwith "Another error occurred"
    end in
  let check_vertical_line (x, y) (x1, y1) (x2, y2) =
    begin
      let (x1', y1') = (float_of_int x1, float_of_int y1) in
      let (x2', y2') = (float_of_int x2, float_of_int y2) in
      let x' = x in
      let y' = y in 

      if x1 = x2 then
        if (min y1' y2') <= y' && y' < (max y1' y2') && x' < x1' then true else false 
      else
        failwith "Another error occurred"
    end in *)
    

  (* let check_vertical (x, y) (x1, y1) (x2, y2) =
      let (x1', y1') = (float_of_int x1, float_of_int y1) in
      let (x2', y2') = (float_of_int x2, float_of_int y2) in
      let x' = x in
      let y' = y in 
      let c1 = (x' < x1') <> (x' < x2') in
      let c2 = y' < y1' +. ( ((x'-.x1') /. (x2'-.x1')) *. (y2'-.y1') ) in
      c1 && c2 in *)


  let check (x, y) (x1, y1) (x2, y2) =
    begin
      (* let (x1', y1') = (float_of_int x1, float_of_int y1) in
      let (x2', y2') = (float_of_int x2, float_of_int y2) in *)

      let (x1', y1') = (x1, y1) in
      let (x2', y2') = (x2, y2) in
      let x' = x in
      let y' = y in 

      (* let x' = float_of_int x in
      let y' = float_of_int y in  *)
      (* (float_of_int y) -. (if y = y1 || y = y2 then 0.1 else 0.0) in *)


      (* if y' < y1' || y' > y2' then false
      else if x >= max x1 x2 then false
      else 
        let m_red = if x1 <> x2 then ((y2' -. y1') /. (x2' -. x1')) else max_float in
        let m_blue = if x1 <> x2 then ((y' -. y1') /. (x' -. x1')) else max_float in
        if m_blue >= m_red then true else false *)

      let c1 = (y' < y1') <> (y' < y2') in
      (* let c2 = x' < x1' in *)
      
      (* let c1 = (y1' <= y' && y' < y2') || (y2' <= y' && y' < y1') in *)
      (* let c2 = x >= max x1 x2 in *)

      (* if y' < y1' || y' > y2' then false *)

      (* Printf.printf "%f\n" t; *)
      let c2 = x' < x1' +. ( ((y'-.y1') /. (y2'-.y1')) *. (x2'-.x1') ) in
      (* let c1 = (y <= y1) <> (y <= y2) in *)
      (* let t = if y2 = y2 then 1 else ((y-y1) / (y2-y1)) in *)
      (* let c2 = x < x1 + ( ((y-y1) / (y2-y1)) * (x2-x1) ) in *)
      (* let c2 = x <= x1 + ( t * (x2-x1) ) in *)

      (* Printf.printf "current: (%d,%d) edge: (%d,%d) (%d,%d) %b %b -> %b\n" x y x1 y1 x2 y2 c1 c2 (c1 && c2); *)
      c1 && c2


      (* Printf.printf "%.2f\n" (y2' -. y1'); *)
      (* (x' -. x1') *. (y2' -. y1') < (x2' -. x1') *. (y' -. y1')  *)

      (* if x1 = x2 then
        if (min y1' y2') <= y' && y' < (max y1' y2') && x' < x1' then true else false 
      else if y1 = y2 then
        false
      else
        failwith "Another error occurred" *)
      end in

      (* if y' < y1' || y' > y2' then false
      else if x < x1 then false
      else true *)
    
  ps
  (* |> List.filter (fun (x, y) -> List.exists (fun (xx, yy) -> x = xx && y = yy) points |> not) *)
  |> List.map (fun (x, y) ->
    (* Printf.printf "visit %d,%d\n" x y; *)
    (* let vertical = edges
    |> List.filter (fun ((x1, y1), (x2, y2)) -> y1 <> y2)
    |> List.map (fun ((x1, y1), (x2, y2)) -> check (x, y) (x1, y1) (x2, y2))
    |> List.filter ((=) true)
    |> List.length
    |> fun x -> x mod 2 = 1 in

    let horizontal = edges
    |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 <> x2)
    |> List.map (fun ((x1, y1), (x2, y2)) -> check (x, y) (y1, x1) (y2, x2))
    |> List.filter ((=) true)
    |> List.length
    |> fun x -> x mod 2 = 1 in

    vertical && horizontal *)

    edges
    |> List.map (fun ((x1, y1), (x2, y2)) -> 
      (* (check (x, y) (x1, y1) (x2, y2)) && (check_vertical (x, y) (x1, y1) (x2, y2)) *)
      check (x, y) (x1, y1) (x2, y2)
    )
    |> List.filter ((=) true)
    |> List.length
    |> fun x -> x mod 2 = 1

    (* let vertical = edges
    |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2)
    |> List.map (fun ((x1, y1), (x2, y2)) -> check_vertical_line (x, y) (x1, y1) (x2, y2))
    |> List.filter ((=) true)
    |> List.length
    |> fun x -> x mod 2 = 1 in

    let horizontal = edges
    |> List.filter (fun ((x1, y1), (x2, y2)) -> y1 = y2)
    |> List.map (fun ((x1, y1), (x2, y2)) -> check_horizontal_line (x, y) (x1, y1) (x2, y2))
    |> List.filter ((=) true)
    |> List.length
    |> fun x -> x mod 2 = 1 in

    vertical && horizontal *)
  )
  |> List.for_all (fun x -> x)
end

let ray_intersect_check (x, y) (x1, y1) (x2, y2) =
  let c1 = (y < y1) <> (y < y2) in
  let c2 = x < x1 +. ( ((y-.y1) /. (y2-.y1)) *. (x2-.x1) ) in
  c1 && c2

let ccw (x1,y1) (x2,y2) (x3,y3) = (y3-.y1)*.(x2-.x1) > (y2-.y1)*.(x3-.x1)
let intersect a b c d = (ccw a c d) <> (ccw b c d) && (ccw a b c) <> (ccw a b d)

let edge_intersect_check edges xs =

  edges
  |> List.map (fun (e1, e2) ->
    xs |> List.map (fun (p1, p2) -> not (intersect p1 p2 e1 e2)) |> List.fold_left (&&) true
  ) 
  |> List.for_all (fun x -> x = true)
  (* xs
  |> List.map (fun (p1, p2) ->
    edges
    |> List.map (fun (e1, e2) -> intersect p1 p1 e1 e2)
    |> List.for_all (fun x -> x = false)
  )
  |> List.for_all (fun x -> x = false) *)



let ee = edges |> List.map (fun ((x1, y1), (x2, y2)) ->
    let (x1', y1') = (float_of_int x1, float_of_int y1) in
    let (x2', y2') = (float_of_int x2, float_of_int y2) in

    ((x1',y1'),(x2',y2'))
)


let input =
  points
  |> create_rec
  |> List.sort (fun (v1, _) (v2, _) -> compare v1 v2)
  |> List.rev
  |> List.find (fun (area, ((x1,y1), (x2,y2))) ->
    (* Printf.printf "checking (%d,%d),(%d,%d) -> %d\n" x1 y1 x2 y2 area; *)

    (* let x1' = float_of_int (max x1 x2) -. 0.5 in
    let y1' = float_of_int (max y1 y2) -. 0.5  in
    let x2' = float_of_int (min x2 x1) +. 0.5 in
    let y2' = float_of_int (min y2 y1) +. 0.5 in *)
    (* let p1 = (x1', y1') in
    let p2 = (x1', y2') in
    let p3 = (x2', y1') in
    let p4 = (x2', y2') in *)

    let p1 = (x1, y1) in
    let p2 = (x1, y2) in
    let p3 = (x2, y1) in
    let p4 = (x2, y2) in
    let ps = [p1; p2; p3; p4] in

    Printf.printf "checking at (%.2f,%.2f)-(%.2f,%.2f) (%.2f,%.2f),(%.2f,%.2f),(%.2f,%.2f),(%.2f,%.2f) -> %d\n" x1 y1 x2 y2 x1 y1 x1 y2 x2 y1 x2 y2 area;

    (* let ps = [p2; p3] in *)
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

    let es = [
      (p1,p2);
      (p2,p4);
      (p4,p3);
      (p3,p1);
    ] in

    (* let okok = ee
    |> List.for_all (fun e ->
      let (x1, y1), (x2, y2) = e in
      not (intersect p1 p2 (x1, y1) (x2, y2)) &&
      not (intersect p2 p4 (x1, y1) (x2, y2)) &&
      not (intersect p4 p3 (x1, y1) (x2, y2)) &&
      not (intersect p3 p1 (x1, y1) (x2, y2))


    ) in *)

    let ok1 = ray_cast_check ee ps in
    let ok2 = edge_intersect_check ee es in 
    let ok = ok1 && ok2 in

    if ok then
      Printf.printf "ok (%.2f,%.2f),(%.2f,%.2f)\n" x1 y1 x2 y2;

    ok
  )
  |> fun (area, _) -> area
  |> printf "Total sum: %d\n"

(* let () = points |> create_rectangles |> List.length |> printf "rectangle count: %d\n" *)

(* let () = Printf.printf "edge size: %d\n" (List.length edges)
let () = Printf.printf "find_align_pair size: %d\n" (points |> find_align_pair |> List.length)

let a1 = edges |> List.sort compare
let a2 = points |> find_align_pair |> List.sort compare

(* let () = Printf.printf "equal: %b\n" (a1 = a2) *)

let () = List.iter (fun ((x1,y1), (x2,y2)) -> Printf.printf "edge: (%d,%d)-(%d,%d)\n" x1 y1 x2 y2; ) a2

let a3 = points @ [List.hd points]
let () = List.iter (fun (x,y) -> Printf.printf "point: (%d,%d)\n" x y; ) a3

let () = points |> create_edges |> List.iter (fun ((x1,y1), (x2,y2)) -> Printf.printf "edge: (%d,%d)-(%d,%d)\n" x1 y1 x2 y2; ) *)