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

let calc_max_seat_id id boarding_pass =
  let row = get_row boarding_pass in
  let col = get_col boarding_pass in
  let new_id = 8 * row + col in
  if new_id > id then new_id else id

let () =
  let max_seat_id = Lib.fold_file_lines "input" calc_max_seat_id 0 in
  Printf.printf "Part 1: %d\n" max_seat_id

(* Part 1: 935 *)
