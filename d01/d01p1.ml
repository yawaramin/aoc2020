(* 1010884 *)
let () =
  let tbl = Hashtbl.create 200 in
  let ch = open_in "input" in
  let continue = ref true in
  while !continue do
    match ch |> input_line |> int_of_string with
    | int ->
      let other_int = 2020 - int in
      begin match Hashtbl.find tbl other_int with
      | () ->
        continue := false;
        Printf.printf "%d\n" (int * other_int)
      | exception Not_found ->
        Hashtbl.add tbl int ()
      end
    | exception End_of_file ->
      continue := false
    | exception _ ->
      ()
  done;
  close_in ch
