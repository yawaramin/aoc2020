type bag = { colour : string; contains : (bag * int) list }

module Parser = struct
  open Angstrom

  let sp = skip_many1 (char ' ')
  let word = take_while1 (function 'a'..'z' -> true | _ -> false)
  let num = take_while1 (function '0'..'9' -> true | _ -> false)

  let coloured_bag =
    let+ adj = word
    and+ _ = sp
    and+ col = word
    and+ _ = sp
    and+ _ = string "bag"
    and+ _ = option 's' (char 's') in
    adj ^ " " ^ col

  let content =
    let+ count = num
    and+ _ = sp
    and+ colour = coloured_bag in
    { colour; contains = [] }, int_of_string count

  let contains_bags = sep_by1 (string ", ") content
  let no_other_bags = let+ _ = string "no other bags" in []

  let bag =
    let+ colour = coloured_bag
    and+ _ = string " contain "
    and+ contains = (contains_bags <|> no_other_bags) in
    { colour; contains }
end

module Bags = Graph.Imperative.Digraph.Concrete(struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let hash = Hashtbl.hash
end)

module BagSearch = Graph.Path.Dijkstra(Bags)(struct
  type edge = Bags.edge
  type t = int

  let weight _ = 1
  let compare = Int.compare
  let add = ( + )
  let zero = 0
end)

let bags = Bags.create ~size:594 ()

let add_bag colour ({ colour = content_colour; _ }, _) =
  Bags.add_vertex bags colour;
  Bags.add_vertex bags content_colour;
  Bags.add_edge bags colour content_colour

let add_bags () = function
  | "" ->
    ()
  | line ->
    match Angstrom.(parse_string ~consume:Consume.Prefix Parser.bag line) with
    | Ok { colour; contains } -> List.iter (add_bag colour) contains
    | Error msg -> failwith ("add_bags" ^ msg ^ ": '" ^ line ^ "'")

let check_bag container count = match container with
  | "shiny gold" ->
    count
  | _ ->
    match BagSearch.shortest_path bags container "shiny gold" with
    | _path -> succ count
    | exception Not_found -> count

let () =
  let () = Lib.fold_file_lines "input" add_bags () in
  let num_shiny_gold_paths = Bags.fold_vertex check_bag bags 0 in
  Printf.printf
    "Part 1: %d
Part 2:\n"
    num_shiny_gold_paths

(* Part 1: 155
   Part 2: *)
