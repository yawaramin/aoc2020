open Lib

let mapx = 31
let mapy = 323

let map = Array.make_matrix mapx mapy 0

let fill_line y (x, char) = match char with
  | '.' -> map.(x).(y) <- 0
  | '#' -> map.(x).(y) <- 1
  | _ -> ()

let fill_row y line =
  line |> String.to_seqi |> Seq.iter (fill_line y);
  succ y

let map_get x y = map.(x mod mapx).(y)

let num_trees_for_slope right down =
  let result = ref 0 in
  for y = 0 to ((mapy - 2) / down) do
    let y_plus_1 = succ y in
    result := !result + map_get (y_plus_1 * right) (y_plus_1 * down)
  done;
  !result

(* Part 1: 220
   Part 2: 2138320800 *)
let () =
  0 |> fold_file_lines "input" fill_row |> ignore;
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    (num_trees_for_slope 3 1)
    (num_trees_for_slope 1 1 *
     num_trees_for_slope 3 1 *
     num_trees_for_slope 5 1 *
     num_trees_for_slope 7 1 *
     num_trees_for_slope 1 2)
