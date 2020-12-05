type partial_passport = {
  byr : string;
  iyr : string;
  eyr : string;
  hgt : string;
  hcl : string;
  ecl : string;
  pid : string;
}

let empty_passport = {
  byr = "";
  iyr = "";
  eyr = "";
  hgt = "";
  hcl = "";
  ecl = "";
  pid = "";
}

let valid_passport { byr; iyr; eyr; hgt; hcl; ecl; pid } =
  byr <> "" &&
  iyr <> "" &&
  eyr <> "" &&
  hgt <> "" &&
  hcl <> "" &&
  ecl <> "" &&
  pid <> ""

let fill_partial partial field = match String.split_on_char ':' field with
  | [_; ""] -> partial
  | ["byr"; byr] -> { partial with byr }
  | ["iyr"; iyr] -> { partial with iyr }
  | ["eyr"; eyr] -> { partial with eyr }
  | ["hgt"; hgt] -> { partial with hgt }
  | ["hcl"; hcl] -> { partial with hcl }
  | ["ecl"; ecl] -> { partial with ecl }
  | ["pid"; pid] -> { partial with pid }
  | _ -> partial

let count_valid (count, partial) = function
  | "" ->
    if valid_passport partial then succ count, empty_passport
    else count, empty_passport
  | line ->
    let fields = String.split_on_char ' ' line in
    count, List.fold_left fill_partial partial fields

let () =
  let num_valid, last_passport =
    Lib.fold_file_lines "input" count_valid (0, empty_passport)
  in
  let num_valid =
    if valid_passport last_passport then succ num_valid else num_valid
  in
  Printf.printf "Part 1: %d\n" num_valid

(* Part 1: 222 *)
