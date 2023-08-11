let file = "./input"

let get_max_calories () =
  let in_ch = open_in file in
  let rec max_calories curr_maxes acc in_ch =
    let line = try Some (input_line in_ch) with End_of_file -> None in
    match line with
    | None -> curr_maxes
    | Some "" ->
        let maxes = List.sort compare (acc :: curr_maxes) in
        let new_maxes =
          if List.length maxes <= 3 then maxes else List.tl maxes
        in
        max_calories new_maxes 0 in_ch
    | Some line ->
        let cal = int_of_string line in
        max_calories curr_maxes (acc + cal) in_ch
  in
  max_calories [] 0 in_ch

let () =
  List.iter
    (fun i -> Printf.printf "Max elf calories: %d\n" i)
    (get_max_calories ())
