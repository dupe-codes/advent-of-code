(* TODO: Move to a utils module *)
let read_input input_file =
  let ic = open_in input_file in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  let _ = close_in ic in
  lines

let is_digit = function '1' .. '9' -> true | _ -> false

(* Not the most efficient, but oh well :] *)
let build_val_from_first_last numbers =
  String.make 1 (List.hd (List.rev numbers)) ^ String.make 1 (List.hd numbers)

type parse_transition = ADVANCE | CONTINUE | DONE

let parse_expected input expected start =
  let rec loop idx =
    if idx >= String.length expected then DONE
    else if idx >= String.length input then CONTINUE
    else if input.[idx] != expected.[idx] then ADVANCE
    else loop (idx + 1)
  in
  loop start

let parse_digit input =
  match input.[0] with
  | 'o' -> parse_expected input "one" 0
  | 't' ->
      if String.length input >= 2 then
        match input.[1] with
        | 'w' -> parse_expected input "two" 2
        | 'h' -> parse_expected input "three" 2
        | _ -> ADVANCE
      else CONTINUE
  | 'f' ->
      if String.length input >= 2 then
        match input.[1] with
        | 'o' -> parse_expected input "four" 2
        | 'i' -> parse_expected input "five" 2
        | _ -> ADVANCE
      else CONTINUE
  | 's' ->
      if String.length input >= 2 then
        match input.[1] with
        | 'i' -> parse_expected input "six" 2
        | 'e' -> parse_expected input "seven" 2
        | _ -> ADVANCE
      else CONTINUE
  | 'e' -> parse_expected input "eight" 0
  | 'n' -> parse_expected input "nine" 0
  | _ -> ADVANCE

let to_digit = function
  | "one" -> '1'
  | "two" -> '2'
  | "three" -> '3'
  | "four" -> '4'
  | "five" -> '5'
  | "six" -> '6'
  | "seven" -> '7'
  | "eight" -> '8'
  | "nine" -> '9'
  | _ -> failwith "Invalid digit"

(*
  TODO: Apparently overlapping values are valid...
        eighthree -> 83
 *)
let parse_digits line =
  let rec parse start curr digits =
    if curr >= String.length line then digits
    else if is_digit line.[curr] then
      parse (curr + 1) (curr + 1) (line.[curr] :: digits)
    else
      let sub = String.sub line start (curr - start + 1) in
      match parse_digit sub with
      | DONE -> parse (start + 1) curr (to_digit sub :: digits)
      | CONTINUE -> parse start (curr + 1) digits
      | ADVANCE ->
          if start = curr then parse (curr + 1) (curr + 1) digits
          else parse (start + 1) curr digits
  in
  parse 0 0 []

let parse_calibration_val line =
  parse_digits line |> build_val_from_first_last |> int_of_string

let calculate_calibration_val input_lines =
  let _ =
    List.iter
      (fun line -> print_endline (string_of_int (parse_calibration_val line)))
      input_lines
  in
  List.map parse_calibration_val input_lines |> List.fold_left ( + ) 0

let _ =
  read_input "bin/day_1/input.txt"
  |> calculate_calibration_val |> string_of_int
  |>
  let _ = print_endline "\nAnswer:" in
  print_endline
