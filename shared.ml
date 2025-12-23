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

let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l;;

let printline a = (print_endline ""; a);;

module IntSet = Set.Make(Int)
let print_set set = 
  IntSet.iter (fun x -> print_int x; print_string " ") set;;
