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

module Bag = struct
  type t = string

  let compare = String.compare
  let equal = String.equal
  let hash = Hashtbl.hash
end

module Bags = Graph.Imperative.Digraph.ConcreteLabeled(Bag)(struct
  type t = int

  let compare = Int.compare
  let default = 0
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

let add_bag colour ({ colour = content_colour; _ }, count) =
  Bags.add_edge_e bags (Bags.E.create colour count content_colour)

let add_bags () = function
  | "" ->
    ()
  | line ->
    match Angstrom.(parse_string ~consume:Consume.Prefix Parser.bag line) with
    | Ok { colour; contains } -> List.iter (add_bag colour) contains
    | Error msg -> failwith ("add_bags" ^ msg ^ ": '" ^ line ^ "'")

let shiny_gold = "shiny gold"

let check_bag container count = match container with
  | "shiny gold" ->
    count
  | _ ->
    match BagSearch.shortest_path bags container shiny_gold with
    | _path -> succ count
    | exception Not_found -> count

let rec count_bags edge count =
  let num = Bags.E.label edge in
  count + num * (1 + count_contents (Bags.E.dst edge) 0)

and count_contents bag = Bags.fold_succ_e count_bags bags bag

let () =
  Lib.fold_file_lines "input" add_bags ();
  let num_shiny_gold_paths = Bags.fold_vertex check_bag bags 0 in
  let num_bags_in_shiny_gold = count_contents shiny_gold 0 in
  Printf.printf
    "Part 1: %d
Part 2: %d\n"
    num_shiny_gold_paths
    num_bags_in_shiny_gold

(* Part 1: 155
   Part 2: 54803 *)
