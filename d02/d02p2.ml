open Lib

let count_valid_passwords count line = match String.split_on_char ':' line with
  | [policy; password] ->
    let password = String.trim password in
    begin match String.split_on_char ' ' policy with
    | [positions; character] ->
      let character = character.[0] in
      begin match String.split_on_char '-' positions with
      | [pos1; pos2] ->
        let pos1 = int_of_string pos1 - 1 in
        let pos2 = int_of_string pos2 - 1 in
        begin match password.[pos1] = character, password.[pos2] = character with
        | false, true
        | true, false -> succ count
        | exception _
        | _ -> count
        end
      | _ ->
        count
      end
    | _ ->
      count
    end
  | _ ->
    count

(* 441 *)
let () =
  let num_valid = fold_file_lines "input" count_valid_passwords 0 in
  Printf.printf "%d\n" num_valid
