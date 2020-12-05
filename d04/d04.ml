type partial_passport = {
  byr : string;
  iyr : string;
  eyr : string;
  hgt : string;
  hcl : string;
  ecl : string;
  pid : string;
}

let pp f { byr; iyr; eyr; hgt; hcl; ecl; pid } =
  Format.fprintf
    f
    "{ byr = %s; iyr = %s; eyr = %s; hgt = %s; hcl = %s; ecl = %s; pid = %s }\n"
    byr
    iyr
    eyr
    hgt
    hcl
    ecl
    pid

let empty_passport = {
  byr = "";
  iyr = "";
  eyr = "";
  hgt = "";
  hcl = "";
  ecl = "";
  pid = "";
}

let valid_passport_p1 { byr; iyr; eyr; hgt; hcl; ecl; pid } =
  byr <> "" &&
  iyr <> "" &&
  eyr <> "" &&
  hgt <> "" &&
  hcl <> "" &&
  ecl <> "" &&
  pid <> ""

let valid_hgt hgt =
  match String.split_on_char 'c' hgt, String.split_on_char 'i' hgt with
  | [cm; "m"], _ ->
    let cm = int_of_string cm in
    cm >= 150 && cm <= 193
  | _, [in_; "n"] ->
    let in_ = int_of_string in_ in
    in_ >= 59 && in_ <= 76
  | _ -> false

let hex_colour_regex =
  Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"

let pid_regex = Str.regexp "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"

let valid_ecl = function
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let valid_passport_p2 ({ byr; iyr; eyr; hgt; hcl; ecl; pid } as partial) =
  valid_passport_p1 partial &&
  let byr = int_of_string byr in
  let iyr = int_of_string iyr in
  let eyr = int_of_string eyr in
  byr >= 1920 && byr <= 2002 &&
  iyr >= 2010 && iyr <= 2020 &&
  eyr >= 2020 && eyr <= 2030 &&
  valid_hgt hgt &&
  Str.string_match hex_colour_regex hcl 0 &&
  valid_ecl ecl &&
  Str.string_match pid_regex pid 0

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

let count_valid (count_p1, count_p2, partial) = function
  | "" ->
    let count_p1 =
      if valid_passport_p1 partial then succ count_p1 else count_p1
    in
    let count_p2 =
      try if valid_passport_p2 partial then succ count_p2 else count_p2 with
      | Failure _ as exn ->
       Format.printf "partial = %a" pp partial;
       raise exn
    in
    count_p1, count_p2, empty_passport
  | line ->
    let fields = String.split_on_char ' ' line in
    count_p1, count_p2, List.fold_left fill_partial partial fields

let () =
  let num_valid_p1, num_valid_p2, last_passport =
    Lib.fold_file_lines "input" count_valid (0, 0, empty_passport)
  in
  let num_valid_p1 =
    if valid_passport_p1 last_passport then succ num_valid_p1 else num_valid_p1
  in
  let num_valid_p2 =
    if valid_passport_p2 last_passport then succ num_valid_p2 else num_valid_p2
  in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    num_valid_p1
    num_valid_p2

(* Part 1: 222
   Part 2: 140 *)
