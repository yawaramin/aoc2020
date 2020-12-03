open Lib

let count_of (character : char) count password_char =
  if character = password_char then succ count else count

let count_valid_passwords count line = match String.split_on_char ':' line with
  | [policy; password] ->
    begin match String.split_on_char ' ' policy with
    | [counts; character] ->
      begin match String.split_on_char '-' counts with
      | [at_least; at_most] ->
        let char_count = password
          |> String.to_seq
          |> Seq.fold_left (count_of character.[0]) 0
        in
        let at_least = int_of_string at_least in
        let at_most = int_of_string at_most in
        if at_least <= char_count && char_count <= at_most then succ count
        else count
      | _ ->
        count
      end
    | _ ->
      count
    end
  | _ ->
    count

(* 467 *)
let () =
  let num_valid = fold_file_lines "input" count_valid_passwords 0 in
  Printf.printf "%d\n" num_valid
