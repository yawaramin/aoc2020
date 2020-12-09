type op = Nop | Acc | Jmp
type instruction = op * int

let size = 628
let program = Array.make size (Nop, 0)
let history = Array.make size false

let instruction_of_string line =
  let len = String.length line in
  let num = int_of_string (String.sub line 5 (len - 5)) in
  let num = match line.[4] with
  | '+' ->
    num
  | '-' ->
    -num
  | ch ->
    let msg = Bytes.of_string "instruction_of_string: invalid sign:  " in
    Bytes.set msg 37 ch;
    failwith (Bytes.to_string msg)
  in
  let op = match String.sub line 0 3 with
    | "nop" -> Nop
    | "acc" -> Acc
    | "jmp" -> Jmp
    | op -> failwith ("instruction_of_string: invalid op: " ^ op)
  in
  op, num

let load_instruction ptr =
  let next_ptr = succ ptr in
  function
  | "" ->
    next_ptr
  | line ->
    program.(ptr) <- instruction_of_string line;
    next_ptr

let rec run ptr acc =
  if not (history.(ptr)) && ptr < size then begin
    history.(ptr) <- true;
    match program.(ptr) with
    | Nop, _ -> run (succ ptr) acc
    | Acc, amt -> run (succ ptr) (acc + amt)
    | Jmp, amt -> run (ptr + amt) acc
  end else
    (* Tuple of: (accumulator, did the program terminate?) *)
    acc, ptr = size

let () =
  ignore (Lib.fold_file_lines "input" load_instruction 0);
  let acc_p1, _ = run 0 0 in
  Printf.printf
    "Part 1: %d
Part 2:\n"
    acc_p1

(* Part 1: 1766
   Part 2: *)
