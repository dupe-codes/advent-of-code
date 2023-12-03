let print_array arr =
  let _ =
    Array.iter
      (fun row ->
        let _ = Array.iter (fun ch -> print_char ch) row in
        print_newline ())
      arr
  in
  let _ = print_newline () in
  arr

let is_symbol = function '0' .. '9' | '.' -> false | _ -> true
let is_digit = function '1' .. '9' -> true | _ -> false

let rec range start stop =
  if start > stop then [] else start :: range (start + 1) stop

(* Loop over row and col displacements, both from -1 to 1 *)
(* NOTE: This is very slow - leads to worst case O(n^3) if every cell is a digit *)
let symbol_adjacent_at schematic row col =
  (*let _ = Printf.printf "Checking %d %d\n" row col in*)
  let rec loop i j =
    if i > 1 then false
    else if j > 1 then loop (i + 1) (-1)
    else
      let new_row = row + i in
      let new_col = col + j in
      if new_row < 0 || new_row >= Array.length schematic then loop i (j + 1)
      else if new_col < 0 || new_col >= Array.length schematic.(0) then
        loop i (j + 1)
      else is_symbol schematic.(new_row).(new_col) || loop i (j + 1)
  in
  loop (-1) (-1)

let symbol_is_adjacent row start_col end_col schematic =
  List.exists (symbol_adjacent_at schematic row) (range start_col (end_col - 1))

let is_part_number num i j schematic =
  num != "" && symbol_is_adjacent i (j - String.length num) j schematic

let find_part_numbers schematic =
  let rec loop i j curr_num nums =
    if i >= Array.length schematic then List.rev nums
    else if j >= Array.length schematic.(0) then
      loop (i + 1) 0 ""
        (if is_part_number curr_num i j schematic then
           int_of_string curr_num :: nums
         else nums)
    else
      match schematic.(i).(j) with
      | '0' .. '9' as ch -> loop i (j + 1) (curr_num ^ Char.escaped ch) nums
      | _ ->
          loop i (j + 1) ""
            (if is_part_number curr_num i j schematic then
               int_of_string curr_num :: nums
             else nums)
  in
  loop 0 0 "" []

let parse_engine_schematic input =
  let num_rows = List.length input in
  let num_cols = String.length (List.hd input) in
  let result = Array.init num_rows (fun _ -> Array.make num_cols '0') in
  let _ =
    List.iteri
      (fun i line -> String.iteri (fun j ch -> result.(i).(j) <- ch) line)
      input
  in
  result

let _ =
  Aoc.Util.read_input "bin/day_3/input.txt"
  |> parse_engine_schematic |> print_array |> find_part_numbers
  |> List.fold_left
       (fun sum num ->
         let _ = print_endline (string_of_int num) in
         sum + num)
       0
  |> Printf.printf "\nPart 1 result: %d\n"
