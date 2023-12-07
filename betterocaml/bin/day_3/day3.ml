module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

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

type coord = { row : int; col : int }

type part_numbers = {
  numbers : int list;
  (* associative list of coords and part num at that coord *)
  coords : (coord * int) list;
}

let print_part_numbers nums =
  let _ = Printf.printf "Part numbers:\n" in
  let _ =
    List.iter (fun num -> print_endline (string_of_int num)) nums.numbers
  in
  let _ = Printf.printf "Coords:\n" in
  let _ =
    List.iter
      (fun (coord, num) -> Printf.printf "%d %d %d\n" coord.row coord.col num)
      nums.coords
  in
  nums

let build_coords_mapping num row end_col =
  List.map
    (fun col -> ({ row; col }, int_of_string num))
    (range (end_col - String.length num) (end_col - 1))

(* Update to also return associative list from coords to number at that coord *)
(* Second pass to find * with part numbers adjacent to them. We can update
   symbol_adjacent_at to take a predicate so it can be reused to search for the
   right astericks *)
let find_part_numbers schematic =
  let rec loop i j curr_num nums =
    if i >= Array.length schematic then nums
    else if j >= Array.length schematic.(0) then
      loop (i + 1) 0 ""
        (if is_part_number curr_num i j schematic then
           {
             numbers = int_of_string curr_num :: nums.numbers;
             coords =
               List.append (build_coords_mapping curr_num i j) nums.coords;
           }
         else nums)
    else
      match schematic.(i).(j) with
      | '0' .. '9' as ch -> loop i (j + 1) (curr_num ^ Char.escaped ch) nums
      | _ ->
          loop i (j + 1) ""
            (if is_part_number curr_num i j schematic then
               {
                 numbers = int_of_string curr_num :: nums.numbers;
                 coords =
                   List.append (build_coords_mapping curr_num i j) nums.coords;
               }
             else nums)
  in
  loop 0 0 "" { numbers = []; coords = [] }

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

let print_int_set set =
  let _ = Printf.printf "Gear numbs: " in
  let _ = IntSet.iter (fun num -> Printf.printf "%d " num) set in
  let _ = Printf.printf "\n" in
  set

let maybe_get_gear_ratio schematic part_coords row col =
  let rec find_adjacent_part_numbers i j part_nums =
    if i > 1 then part_nums
    else if j > 1 then find_adjacent_part_numbers (i + 1) (-1) part_nums
    else
      let new_row = row + i in
      let new_col = col + j in
      if new_row < 0 || new_row >= Array.length schematic then
        find_adjacent_part_numbers i (j + 1) part_nums
      else if new_col < 0 || new_col >= Array.length schematic.(0) then
        find_adjacent_part_numbers i (j + 1) part_nums
      else
        match List.assoc_opt { row = new_row; col = new_col } part_coords with
        | Some part_num ->
            find_adjacent_part_numbers i (j + 1) (IntSet.add part_num part_nums)
        | None -> find_adjacent_part_numbers i (j + 1) part_nums
  in
  let adjacent_part_nums =
    find_adjacent_part_numbers (-1) (-1) IntSet.empty |> print_int_set
  in
  match IntSet.cardinal adjacent_part_nums with
  | 2 -> Some (IntSet.fold (fun acc num -> acc * num) adjacent_part_nums 1)
  | _ -> None

let find_gear_ratios schematic part_coords =
  let rec loop i j ratios =
    if i >= Array.length schematic then ratios
    else if j >= Array.length schematic.(0) then loop (i + 1) 0 ratios
    else
      match schematic.(i).(j) with
      | '*' -> (
          match maybe_get_gear_ratio schematic part_coords i j with
          | Some ratio -> loop i (j + 1) (ratio :: ratios)
          | None -> loop i (j + 1) ratios)
      | _ -> loop i (j + 1) ratios
  in
  loop 0 0 []

let print_gears gears =
  let _ = Printf.printf "Gear ratios:\n" in
  let _ = List.iter (fun gear -> Printf.printf "%d\n" gear) gears in
  gears

let _ =
  let engine_schematic =
    Aoc.Util.read_input "bin/day_3/input.txt"
    |> parse_engine_schematic |> print_array
  in
  let part_nums = find_part_numbers engine_schematic in
  let gear_ratios =
    find_gear_ratios engine_schematic part_nums.coords |> print_gears
  in
  Printf.printf "Part 2: %d\n" (List.fold_left ( + ) 0 gear_ratios)
