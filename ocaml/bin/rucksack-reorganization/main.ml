module CS = Set.Make (Char)

let rec take k l =
  match k with
  | 0 -> []
  | k -> ( match l with [] -> [] | h :: t -> h :: take (k - 1) t)

(* Assumes that there is always exactly one overlapping element *)
let get_overlap_element first_compartment second_compartment =
  CS.choose (CS.inter first_compartment second_compartment)

let get_priority item =
  let is_uppercase = item = Char.uppercase_ascii item in
  if is_uppercase then Char.code item - Char.code 'A' + 27
  else Char.code item - Char.code 'a' + 1

let get_compartment_items k items =
  let compartment = CS.empty in
  take k items |> fun compartment_items ->
  List.fold_right CS.add compartment_items compartment

let get_rucksack_overlap_priority rucksack =
  let items = List.init (String.length rucksack) (String.get rucksack) in
  let mid = String.length rucksack / 2 in
  let first_compartment = get_compartment_items mid items in
  let second_compartment = get_compartment_items mid (List.rev items) in
  let overlap = get_overlap_element first_compartment second_compartment in
  get_priority overlap

let get_total_priority input_file =
  let rucksacks =
    Stdio.In_channel.read_all input_file |> Str.split (Str.regexp "\n")
  in
  let priorities = List.map get_rucksack_overlap_priority rucksacks in
  List.fold_left ( + ) 0 priorities

let input = "./input"

let () =
  let total_priority = get_total_priority input in
  Printf.printf "Total priority: %d\n" total_priority
