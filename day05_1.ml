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

let input = read_file "day05_input.txt"
  |> String.concat "#"
  |> Str.split (Str.regexp "##")

let fresh_ranges = (List.hd input)
  |> String.split_on_char '#'
  |> List.map (fun r ->
    let split = String.split_on_char '-' r in
    let range_start = int_of_string (List.nth split 0) in
    let range_end = int_of_string (List.nth split 1) in
    (range_start, range_end)
  )
  |> List.sort (fun (k1, v1) (k2, v2) ->
    if k1 = k2 then compare v1 v2
    else compare k1 k2
  )

let ingredients = List.nth input 1
  |> String.split_on_char '#'
  |> List.map int_of_string 
  |> List.sort compare

let rec count_fresh ranges xs = 
  let lr = List.length ranges in
  let lx = List.length xs in

  if lr == 0 || lx == 0 then 0
  else
    let (range_start, range_end) = List.hd ranges in
    let x = List.hd xs in
    if x < range_start then
      (count_fresh ranges (List.tl xs))
    else if range_start <= x && x <= range_end then
      1 + (count_fresh ranges (List.tl xs))
    else if x > range_start then
      count_fresh (List.tl ranges) xs
    else
      count_fresh (List.tl ranges) xs

let () = fresh_ranges
|> List.iter (fun (l, r) -> Printf.printf "[%d-%d]\n" l r)

let () = ingredients
|> List.iter (fun x -> Printf.printf "[i = %d]\n" x)

let () = count_fresh fresh_ranges ingredients
|> printf "Total sum: %d\n"
