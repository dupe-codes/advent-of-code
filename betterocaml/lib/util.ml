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
