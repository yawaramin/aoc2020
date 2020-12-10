type op = Nop | Acc | Jmp
type instruction = op * int

let size = 628
let program = Array.make size (Nop, 0)
let history () = Array.make size false

let instruction_of_string line = match String.split_on_char ' ' line with
  | [op; num] ->
    let num = try int_of_string num with
      | Failure _ -> failwith ("instruction_of_string: bad number: " ^ num)
    in
    let op = match op with
    | "nop" -> Nop
    | "acc" -> Acc
    | "jmp" -> Jmp
    | _ -> failwith ("instruction_of_string: bad op: " ^ op)
    in
    op, num
  | _ ->
    failwith ("instruction_of_string: bad input: " ^ line)

let load_instruction ptr =
  let next_ptr = succ ptr in
  function
  | "" ->
    next_ptr
  | line ->
    program.(ptr) <- instruction_of_string line;
    next_ptr

let eval ~ptr ~acc = function
  | Nop, _ -> succ ptr, acc
  | Acc, amt -> succ ptr, acc + amt
  | Jmp, amt -> ptr + amt, acc

let rec run ?flip (ptr, acc) history =
  if ptr < size && not (history.(ptr)) then begin
    history.(ptr) <- true;
    match program.(ptr), flip with
    | (Nop, amt), Some fliptr when ptr = fliptr ->
      run ?flip (eval ~ptr ~acc (Jmp, amt)) history
    | (Jmp, amt), Some fliptr when ptr = fliptr ->
      run ?flip (eval ~ptr ~acc (Nop, amt)) history
    | instruction, _ ->
      run ?flip (eval ~ptr ~acc instruction) history
  end else
    (* Tuple of: (accumulator, did the program terminate?) *)
    acc, ptr = size

let rec find_terminator ptr =
  if ptr = size then begin
    failwith "find_terminator: could not find instruction to flip"
  end;
  let history = history () in
  match run ~flip:ptr (0, 0) history with
  | acc, true -> acc
  | _, false -> find_terminator (succ ptr)

let () =
  ignore (Lib.fold_file_lines "input" load_instruction 0);
  let acc_p1, _ = run (0, 0) (history ()) in
  let acc_p2 = find_terminator 0 in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    acc_p1
    acc_p2

(* Part 1: 1766
   Part 2: 1639 *)
