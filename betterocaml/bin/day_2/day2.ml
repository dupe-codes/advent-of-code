type cubes = { red : int; green : int; blue : int }

let print_strs strs = List.iter (fun s -> Printf.printf "%s\n" s) strs

let print_cubes cubes_list =
  List.iter
    (fun cubes ->
      Printf.printf "{%d red ; %d green ; %d blue }\n" cubes.red cubes.green
        cubes.blue)
    cubes_list

let get_int_or_default = function Some x -> int_of_string x | None -> 0

(*
  Example draw:
    1 blue, 9 red, 15 green
*)
let parse_draw draw =
  let colors =
    Str.split (Str.regexp ", ") draw
    |> List.map (fun color_group ->
           let parsed_color = Str.split (Str.regexp " ") color_group in
           (List.nth parsed_color 1, List.nth parsed_color 0))
  in
  {
    red = List.assoc_opt "red" colors |> get_int_or_default;
    green = List.assoc_opt "green" colors |> get_int_or_default;
    blue = List.assoc_opt "blue" colors |> get_int_or_default;
  }

(*
  Example line:
    Game 74: 1 blue, 9 red, 15 green; 3 blue, 7 green; 9 green, 1 blue, 7 red
*)
let parse_game line =
  let game_id_and_draws = Str.split (Str.regexp ": ") line in
  let game_id = List.nth game_id_and_draws 0 in
  let id = int_of_string (List.nth (Str.split (Str.regexp " ") game_id) 1) in
  let draws_str = List.nth game_id_and_draws 1 in
  let draws = Str.split (Str.regexp "; ") draws_str in
  (id, List.map parse_draw draws)

let is_possible_draw available drawn =
  drawn.red <= available.red
  && drawn.green <= available.green
  && drawn.blue <= available.blue

let sum_possible_games cubes input =
  let rec loop lines sum =
    match lines with
    | [] -> sum
    | line :: rest ->
        let id, game_cubes = parse_game line in
        let _ = print_cubes game_cubes in
        if List.for_all (is_possible_draw cubes) game_cubes then
          loop rest (sum + id)
        else loop rest sum
  in
  loop input 0

let _ =
  Aoc.Util.read_input "bin/day_2/in"
  |> sum_possible_games { red = 12; green = 13; blue = 14 }
  |> string_of_int |> Printf.printf "Result: %s"
