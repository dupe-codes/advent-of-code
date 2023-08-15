type move = Rock | Paper | Scissors
type outcome = Win | Draw | Lose

let opp_encoding_to_move move =
  match move with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "Invalid move"

let player_encoding_to_move move =
  match move with
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "Invalid move"

(*let get_move_score move =*)
(*match move with Rock -> 1 | Paper -> 2 | Scissors -> 3*)

let input = "./input"

(* 6 -> win, 3 -> draw, 0 -> loss*)
let process_line line =
  let split_line = Str.split (Str.regexp " ") line in
  let opponent_move = opp_encoding_to_move (List.nth split_line 0) in
  let player_move = player_encoding_to_move (List.nth split_line 1) in
  let result =
    match (opponent_move, player_move) with
    | Rock, Win -> 8
    | Rock, Lose -> 3
    | Rock, Draw -> 4
    | Paper, Win -> 9
    | Paper, Lose -> 1
    | Paper, Draw -> 5
    | Scissors, Win -> 7
    | Scissors, Lose -> 2
    | Scissors, Draw -> 6
  in
  result

let get_strategy_score input_file =
  let in_ch = open_in input_file in
  let rec calculate_score () =
    let line = try Some (input_line in_ch) with End_of_file -> None in
    match line with
    | None -> 0
    | Some line -> process_line line + calculate_score ()
  in
  calculate_score ()

let () = Printf.printf "%d\n" (get_strategy_score input)
