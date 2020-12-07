let fold_file_lines name f init =
  let ch = open_in name in
  let continue = ref true in
  let result = ref init in
  while !continue do
    match input_line ch with
    | line ->
      result := f !result line
    | exception End_of_file ->
      result := f !result "";
      continue := false
  done;
  close_in ch;
  !result
