let input_size = 872
let all_seat_ids = Array.make input_size ~-1

let get_row boarding_pass = 7
  |> String.sub boarding_pass 0
  |> String.to_seq
  |> Seq.map (function
    | 'F' -> '0'
    | 'B' -> '1'
    | _ -> failwith "get_row: invalid char")
  |> String.of_seq
  |> (( ^ ) "0b")
  |> int_of_string

let get_col boarding_pass = 3
  |> String.sub boarding_pass 7
  |> String.to_seq
  |> Seq.map (function
    | 'L' -> '0'
    | 'R' -> '1'
    | _ -> failwith "get_col: invalid char")
  |> String.of_seq
  |> (( ^ ) "0b")
  |> int_of_string

let seat_id row col = 8 * row + col

let idx = ref 0

let calc_max_seat_id id boarding_pass =
  let row = get_row boarding_pass in
  let col = get_col boarding_pass in
  let new_id = seat_id row col in
  all_seat_ids.(!idx) <- new_id;
  incr idx;
  if new_id > id then new_id else id

let calc_my_seat_id init =
  let idx = ref 1 in
  let continue = ref true in
  let result = ref init in
  while !continue do
    if !idx = input_size - 1 then (continue := false);
    if all_seat_ids.(!idx) - 2 = all_seat_ids.(!idx - 1) then begin
      result := all_seat_ids.(!idx) - 1;
      continue := false
    end;
    incr idx
  done;
  !result

let () =
  let max_seat_id = Lib.fold_file_lines "input" calc_max_seat_id 0 in
  Array.sort compare all_seat_ids;
  let my_seat_id = calc_my_seat_id 0 in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    max_seat_id
    my_seat_id

(* Part 1: 935
   Part 2: 743 *)
