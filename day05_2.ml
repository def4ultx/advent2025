open Printf
#use "topfind";;
#require "str";;
open Str;;

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

let max a b = if a > b then a else b

(*
1-2 * 5-7 => [1-2, 3-7]
1-5 * 3-7 => [1-7]
*)
let rec merge_range xs =
  match xs with
  | [] -> []
  | x :: [] -> [x]
  | x :: y :: t ->
    let (left_from, left_to) = x in
    let (right_from, right_to) = y in
    if left_to >= right_from then
      let r = (left_from, (max left_to right_to)) in
      merge_range (r :: t)
    else
      x :: (merge_range (y :: t))


let () = read_file "day05_input.txt"
  |> String.concat "#"
  |> Str.split (Str.regexp "##")
  |> List.hd
  |> String.split_on_char '#'
  |> List.map (fun r ->
    let split = String.split_on_char '-' r in
    let range_start = int_of_string (List.nth split 0) in
    let range_end = int_of_string (List.nth split 1) in
    (range_start, range_end)
  )
  |> List.sort (fun (k1, v1) (k2, v2) -> if k1 = k2 then compare v1 v2 else compare k1 k2)
  |> merge_range
  |> List.map (fun (l,r) ->
    begin
      Printf.printf "[%d-%d]\n" l r;
      (l,r)
    end  
  )
  |> List.map (fun (l,r) -> r-l+1)
  |> List.fold_left (+) 0
  |> printf "Total sum: %d\n"
