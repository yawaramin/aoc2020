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

let map_get (x, y) = map.(x mod mapx).(y mod mapy)

(* 220 *)
let () =
  0 |> fold_file_lines "input" fill_row |> ignore;
  let num_trees = ref 0 in
  for y = 1 to (mapy - 1) do
    num_trees := !num_trees + map_get (3 * y, y)
  done;
  Printf.printf "%d\n" !num_trees
