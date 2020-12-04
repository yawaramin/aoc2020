val fold_file_lines : string -> ('a -> string -> 'a) -> 'a -> 'a
(** [fold_file_lines name f init] is the result of applying [f value line] for
    each [line] in file [name], where [value] is the result of the previous
    application. Initially set to [init]. *)
