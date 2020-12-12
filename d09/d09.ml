module Window : sig
  type t

  val make : int -> int array -> t
  val push : t -> int -> unit
  val is_sum2 : int -> t -> bool
end = struct
  type t = { arr : int array; len : int; mutable idx : int }

  let make len arr =
    assert (Array.length arr = len);
    { arr; len; idx = 0 }

  let push t int =
    let idx = succ t.idx mod t.len in
    t.arr.(idx) <- int;
    t.idx <- idx

  let is_sum arr num arr_num = Array.mem (num - arr_num) arr
  let is_sum2 num t = Array.exists (is_sum t.arr num) t.arr
end

let input_len = 1_000
let preamble_len = 25
let numbers = Array.make input_len 0

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

let sum_array = Array.fold_left ( + ) 0

let rec fold_array ~result ~start ~stop f arr =
  if start = stop then
    result
  else
    fold_array ~result:(f result arr.(start)) ~start:(succ start) ~stop f arr

let fold_array f arr = match Array.length arr with
  | 0 -> failwith "fold_array: empty array"
  | 1 -> arr.(0)
  | len -> fold_array ~result:arr.(0) ~start:1 ~stop:len f arr

let min_array = fold_array min
let max_array = fold_array max

let rec get_weakness ?(first=0) ?(last=0) num =
  let len = last - first + 1 in
  if last = input_len then
    failwith "get_weakness: unable to find weakness"
  else if len <= 0 then
    let msg =
      Printf.sprintf "get_weakness: invalid array slice: %d to %d" first last
    in
    failwith msg
  else if len = 1 then
    get_weakness ~first ~last:(succ last) num
  else
    let slice = Array.sub numbers first len in
    let sum = sum_array slice in
    if sum > num then get_weakness ~first:(succ first) ~last num
    else if sum < num then get_weakness ~first ~last:(succ last) num
    else min_array slice + max_array slice

let () =
  ignore (Lib.fold_file_lines "input" fill_numbers 0);
  let window = Window.make preamble_len (Array.sub numbers 0 preamble_len) in
  let first_nonsum = get_first_nonsum window in
  let weakness = get_weakness first_nonsum in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    first_nonsum
    weakness

(* Part 1: 217430975
   Part 2: 28509180 *)
