(* One person's Qs: Questions.t
   One group's Qs: Questions.t list
   All groups' Qs: Questions.t list list *)

module Questions = Set.Make(Char)

let questions_by_group groups = function
  | "" ->
    [] :: groups
  | line ->
    let questions = Questions.add_seq (String.to_seq line) Questions.empty in
    match groups with
    | [] -> [[questions]]
    | group :: groups -> (questions :: group) :: groups

let sum_group_any count group =
  count + Questions.cardinal (List.fold_left Questions.union Questions.empty group)

let sum_group_all count = function
  | [] -> count
  | first :: rest ->
    count + Questions.cardinal (List.fold_left Questions.inter first rest)

let () =
  let all_groups = Lib.fold_file_lines "input" questions_by_group [] in
  let sum_any_questions = List.fold_left sum_group_any 0 all_groups in
  let sum_all_questions = List.fold_left sum_group_all 0 all_groups in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
  sum_any_questions
  sum_all_questions

(* Part 1: 6583
   Part 2: 3290 *)
