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
  
  (* Convert to list and sort by distance *)
  let result = PairMap.fold (fun k v acc -> (k, v) :: acc) unique_dist_list [] in
  List.sort (fun (_, d1) (_, d2) -> compare d1 d2) result

let find_min_distance link_options existing_links =
  (* Skip already linked pairs *)
  let rec skip_linked = function
    | [] -> failwith "No options available"
    | ((v, _) as x) :: rest ->
        if List.mem v existing_links then skip_linked rest
        else (x, rest)
  in
  let (first, remaining) = skip_linked link_options in
  let td = snd first in
  
  (* Collect all matches with same distance *)
  let rec collect_matches acc = function
    | [] -> (List.rev acc, [])
    | ((_, d) as x) :: rest ->
        if d = td then collect_matches (x :: acc) rest
        else (List.rev acc, x :: rest)
  in
  let (matches, rest) = collect_matches [first] remaining in
  
  match matches with
  | [(x, _)] -> (rest, x)
  | _ -> failwith "Multiple matches at same distance"

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

let show_dists dists =
  List.iter
    (fun ((a, b), d) ->
      printf "%s -> %s: %d\n" (string_of_int (let (x,y,z) = a in x)) (string_of_int (let (x,y,z) = b in x)) d)
    dists

let show_circuits circuits =
  List.iter
    (fun c ->
      printf "[";
      List.iter (fun (x,y,z) -> printf "(%d,%d,%d) " x y z) c;
      printf "]\n")
    circuits

let rec connect distances limit iteration circuits links =
  if iteration < limit then begin
    let (distances', pair) = find_min_distance distances links in
    let circuits' = merge pair circuits in
    let links' = pair :: links in
    printf "%d: %d circuits\n%!" iteration (List.length circuits');
    connect distances' limit (iteration + 1) circuits' links'
  end
  else
    circuits

let () =
  let content = In_channel.with_open_text "input-day08.txt" In_channel.input_all in
  let lines_of_file = String.split_on_char '\n' content |> List.filter (fun l -> l <> "") in
  let boxes = List.map parse_line lines_of_file in
  let distances = all_distances boxes in
  (* show_dists distances; *)
  let circuits = connect distances 1000 0 (List.map (fun b -> [b]) boxes) [] in
  printf "Connected %d circuits\n" (List.length circuits);
  let sizes = List.map List.length circuits in
  let sorted_sizes = List.sort (fun a b -> compare b a) sizes in
  let top3 = match sorted_sizes with
    | a :: b :: c :: _ -> a * b * c
    | _ -> failwith "Not enough circuits"
  in
  printf "%d\n" top3
