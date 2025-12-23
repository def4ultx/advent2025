(* open Printf *)

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
  xs
  |> List.map parse_indicator
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)


let parse_joltage line =
  let n = String.length line in
  String.sub line 1 (n-2)
  |> String.split_on_char ','
  |> List.map int_of_string

let parse_input line =
  let indicator = parse_indicator (List.hd line) in
  let schematics = parse_schematic (List.tl (List.rev (List.tl line))) in
  let joltage = parse_joltage (List.hd (List.rev line)) in
  (indicator, schematics, joltage)


let rec transpose matrix =
  if matrix = [] || List.hd matrix = [] then []
  else
    (List.map List.hd matrix) :: transpose (List.map List.tl matrix)

let least_toggle (indicator, schematics, joltage) =
begin
  (* Printf.printf "start of %s\n" indicator; *)

  (* let to_key xs = String.concat "," (List.map string_of_int xs) in *)

  let open Lp in
  let variables = List.init (List.length schematics) (fun i -> Lp.var ~integer:true ~lb:0.0 (Printf.sprintf "x%d" i)) in
  let objective = Lp.minimize (List.fold_left (fun a x -> a ++ x) (List.hd variables) (List.tl variables)) in

  let matrix = Array.make_matrix (List.length schematics) (String.length indicator) 0 in  
  List.iteri (fun i scheme ->
    (* Printf.printf "sch=%s\n" (to_key scheme); *)
    List.iteri (fun _ idx ->
      matrix.(i).(idx) <- 1;

      (* Printf.printf "%d " matrix.(i).(idx); *)
    ) scheme;
    (* print_endline ""; *)
  ) schematics;

  (* Array.iteri (fun _ xs ->
    Array.iteri (fun _ v ->
      Printf.printf "%d " v;
    ) xs;
    print_endline "";
  ) matrix; *)

  let vars = Array.of_list variables in
  let constraints = matrix 
  |> Array.to_list
  |> List.map Array.to_list
  |> transpose
  |> List.mapi (fun i xs ->
    let consts = xs |> List.mapi (fun i x -> (x, vars.(i))) |> List.filter (fun (ok, _) -> ok = 1) |> List.map (fun (_, x) -> x ) in
    let result = List.fold_left (fun a x -> a ++ x) (List.hd consts) (List.tl consts) in
    result =~ (c (float_of_int (List.nth joltage i)))
  ) in
  (* let non_negative_constraints = List.map (fun v -> v >~ c 0.0) variables in
  let all_constraints = equation_constraints @ non_negative_constraints in *)
  let problem = make objective constraints in
  if Lp.validate problem then
    match Lp_glpk.solve problem with
    | Ok (obj, _) -> obj
    | Error _ -> 0.0
  else 0.0
end


let () =
  read_file "day10_input_example.txt"
  |> List.map (String.split_on_char ' ')
  |> List.map parse_input
  |> List.map least_toggle
  |> List.map (fun x -> Printf.printf "%f\n" x; x)
  |> List.fold_left (+.) 0.0
  |> Printf.printf "Total sum: %f\n"




(* var a >= 0;
var b >= 0;
var c >= 0;
var d >= 0;
var e >= 0;
var f >= 0;


minimize z: a + b + c + d + e + f;

subject to c11: a + b = 3;
subject to c12: a + e = 4;
subject to c13: b + c + d = 5;
subject to c14: c + e + f = 7;

end;

(0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}

[1 0 1 1 1]
[0 0 1 1 0]
[1 0 0 0 1]
[1 1 1 0 0]
[0 1 1 1 1]

=

subject to c11: a + c + d = 7;
subject to c12: d + e = 5;
subject to c13: a + b + d + e = 12;
subject to c14: a + b + e = 7;
subject to c15: a + c + e = 2;

[7 5 12 7 2] *)