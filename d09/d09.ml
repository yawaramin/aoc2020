let preamble_len = 25

module Window : sig
  type t

  val make : int array -> t
  val push : t -> int -> unit
  val is_sum2 : int -> t -> bool
end = struct
  type t = { arr : int array; mutable idx : int }

  let make arr =
    assert (Array.length arr = preamble_len);
    { arr; idx = 0 }

  let push t int =
    let idx = succ t.idx mod preamble_len in
    t.arr.(idx) <- int;
    t.idx <- idx

  let is_sum arr num arr_num = Array.mem (num - arr_num) arr
  let is_sum2 num t = Array.exists (is_sum t.arr num) t.arr
end

let numbers = Array.make 1000 0

let fill_numbers idx = function
  | "" ->
    idx
  | line ->
    let number = try int_of_string line with
      | Failure _ -> failwith ("fill_numbers: invalid number: " ^ line)
    in
    numbers.(idx) <- number;
    succ idx

let rec get_first_nonsum ?(idx=preamble_len) window =
  let num = numbers.(idx) in
  if Window.is_sum2 num window then begin
    Window.push window num;
    get_first_nonsum ~idx:(succ idx) window
  end else
    num

let () =
  ignore (Lib.fold_file_lines "input" fill_numbers 0);
  let window = Window.make (Array.sub numbers 0 preamble_len) in
  let first_nonsum = get_first_nonsum window in
  Printf.printf
    "Part 1: %d
Part 2: TODO\n"
    first_nonsum

(* Part 1: 217430975
   Part 2: TODO *)
