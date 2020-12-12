let num_adapters = 94
let adapters = Array.make num_adapters 0

let load_adapter idx = function
  | "" -> idx
  | line ->
    adapters.(idx) <- int_of_string line;
    succ idx

let calc_diffs (curr_adapter, d1j, d3j) next_adapter =
  match next_adapter - curr_adapter with
  | 1 ->
    next_adapter, succ d1j, d3j
  | 3 ->
    next_adapter, d1j, succ d3j
  | _ ->
    let msg = Printf.sprintf
      "calc_diffs: invalid diff between %d and %d"
      curr_adapter
      next_adapter
    in
    failwith msg

let () =
  ignore (Lib.fold_file_lines "input" load_adapter 0);
  Array.sort compare adapters;
  let _, d1j, d3j = Array.fold_left calc_diffs (0, 0, 0) adapters in
  Printf.printf
    "Part 1: %d
Part 2: TODO\n"
    (d1j * succ d3j)

(* Part 1: 1914
   Part 2: TODO *)
