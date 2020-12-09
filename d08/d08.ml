type instruction = Nop | Acc of int | Jmp of int

let size = 628
let program = Array.make size Nop
let history = Hashtbl.create size

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
  match String.sub line 0 3 with
  | "nop" -> Nop
  | "acc" -> Acc num
  | "jmp" -> Jmp num
  | op -> failwith ("instruction_of_string: invalid op: " ^ op)

let load_instruction ptr =
  let next_ptr = succ ptr in
  function
  | "" ->
    next_ptr
  | line ->
    program.(ptr) <- instruction_of_string line;
    next_ptr

let () =
  ignore (Lib.fold_file_lines "input" load_instruction 0);
  let acc = ref 0 in
  let ptr = ref 0 in
  while not (Hashtbl.mem history !ptr) && !ptr < size do
    let ptr_val = !ptr in
    Hashtbl.add history ptr_val ();
    match program.(ptr_val) with
    | Nop ->
      incr ptr
    | Acc amt ->
      incr ptr;
      acc := !acc + amt
    | Jmp amt -> ptr := ptr_val + amt
  done;
  Printf.printf
    "Part 1: %d
Part 2:\n"
    !acc
