open Printf

module IntMap = Map.Make(struct type t = int * int * int let compare = compare end)
module PairMap = Map.Make(struct type t = (int * int * int) * (int * int * int) let compare = compare end)

type vec3 = int * int * int
type pair = vec3 * vec3
type circuit = vec3 list
type dist_list = (pair * int) list

let to_pair a b =
  if a < b then (a, b) else (b, a)

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> failwith "Invalid input format"

let sq x = x * x

let dist_sq (z1, y1, x1) (z2, y2, x2) =
  sq (z1 - z2) + sq (y1 - y2) + sq (x1 - x2)

let all_distances boxes =
  (* Create base map with empty maps for each box *)
  let base_map = List.fold_left (fun acc e -> IntMap.add e IntMap.empty acc) IntMap.empty boxes in
  
  (* Build distance map *)
  let dist_map =
    List.fold_left
      (fun acc e ->
        let dists =
          List.fold_left
            (fun acc_dists o ->
              if e = o then acc_dists
              else IntMap.add o (dist_sq e o) acc_dists)
            (IntMap.find e acc)
            boxes
        in
        IntMap.add e dists acc)
      base_map
      boxes
  in
  
  (* Convert to list of pairs with distances *)
  let dist_list =
    IntMap.fold
      (fun a dists acc ->
        IntMap.fold
          (fun b dist acc' -> (to_pair a b, dist) :: acc')
          dists
          acc)
      dist_map
      []
  in
  
  (* Remove duplicates *)
  let unique_dist_list =
    List.fold_left
      (fun acc (v, d) ->
        if PairMap.mem v acc then acc
        else PairMap.add v d acc)
      PairMap.empty
      dist_list
  in
  
  (* Convert to list and sort by first Vec3 descending *)
  let result = PairMap.fold (fun k v acc -> (k, v) :: acc) unique_dist_list [] in
  List.sort (fun ((_, _), d1) ((_, _), d2) -> compare d1 d2) result

let merge (a, b) circuits =
  (* Find circuit containing a *)
  let (c_a, rest) = List.partition (List.mem a) circuits in
  let c_a = List.hd c_a in
  
  (* Check if b is already in the same circuit *)
  if List.mem b c_a then
    circuits
  else
    (* Find circuit containing b *)
    let (c_b, rest') = List.partition (List.mem b) rest in
    let c_b = List.hd c_b in
    (* Merge the two circuits *)
    (c_a @ c_b) :: rest'

let rec connect_pair distances limit iteration circuits links =
  if iteration < limit then
    match distances with
    | ((pair, _) :: xs) ->
        connect_pair xs limit (iteration + 1) (merge pair circuits) links
    | [] -> circuits
  else
    circuits

let my_long_string = String.concat "\n" [
  "162,817,812";
  "57,618,57";
  "906,360,560";
  "592,479,940";
  "352,342,300";
  "466,668,158";
  "542,29,236";
  "431,825,988";
  "739,650,466";
  "52,470,668";
  "216,146,977";
  "819,987,18";
  "117,168,530";
  "805,96,715";
  "346,949,466";
  "970,615,88";
  "941,993,340";
  "862,61,35";
  "984,92,344";
  "425,690,689"
]

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

let () =
  let lines_of_file = read_file "day08_input.txt" in
  let boxes = List.map parse_line lines_of_file in
  let distances = all_distances boxes in

  List.iter
    (fun ((a, b), d) ->
      let (x1, y1, z1) = a in
      let (x2, y2, z2) = b in
      printf "%d %d %d -> %d %d %d: %d\n" x1 y1 z1 x2 y2 z2 d;
    ) 
    distances;

  let circuits = connect_pair distances 1000 0 (List.map (fun b -> [b]) boxes) [] in
  printf "Connected %d circuits\n" (List.length circuits);
  let sizes = List.map List.length circuits in
  let sorted_sizes = List.sort (fun a b -> compare b a) sizes in
  let top3 = match sorted_sizes with
    | a :: b :: c :: _ -> a * b * c
    | _ -> failwith "Not enough circuits"
  in
  printf "%d\n" top3