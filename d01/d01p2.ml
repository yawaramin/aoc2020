let check int2 int1 int3 () action =
  if int1 + int2 + int3 = 2020 then `Stop (int1, int2, int3)
  else action

let update int2 int1 tbl2 action =
  if int1 + int2 >= 2020 then
    action
  else if Hashtbl.mem tbl2 int2 then
    action
  else begin
    Hashtbl.add tbl2 int2 ();
    Hashtbl.fold (check int2 int1) tbl2 action
  end

let add tbl int =
  if Hashtbl.mem tbl int then
    `Continue
  else begin
    Hashtbl.add tbl int (Hashtbl.create 20);
    Hashtbl.fold (update int) tbl `Continue
  end

(* 253928438 *)
let () =
  let tbl = Hashtbl.create 200 in
  let ch = open_in "input" in
  let continue = ref true in
  while !continue do
    match ch |> input_line |> int_of_string with
    | int ->
      begin match add tbl int with
      | `Continue ->
        ()
      | `Stop (int1, int2, int3) ->
        continue := false;
        Printf.printf "%d\n" (int1 * int2 * int3)
      end
    | exception End_of_file ->
      continue := false
    | exception _ ->
      ()
  done;
  close_in ch
