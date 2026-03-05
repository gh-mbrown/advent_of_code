let file_name = "input/day05.txt"

let parse_data =
    lazy
      (let rec aux range ids = function
           | [] -> range, List.concat ids
           | x :: xs ->
               (match x with
                | [ start; stop ] -> aux ((start, stop) :: range) ids xs
                | [ _ ] -> aux range (x :: ids) xs
                | _ -> failwith "does not match pattern")
       in
       Common.File.read_file_list file_name
       |> List.map (fun x -> String.split_on_char '-' x |> List.map int_of_string)
       |> aux [] [])
;;

let rec valid_ids ids = function
    | [] -> ids
    | (start, stop) :: xs ->
        (match ids with
         | [] -> valid_ids [ start, stop ] xs
         | (start2, stop2) :: ys ->
             (match start with
              | s when s <= stop2 + 1 ->
                  valid_ids ((min s start2, max stop stop2) :: ys) xs
              | s -> valid_ids ((s, stop) :: ids) xs))
;;

let part_one () =
    Lazy.force parse_data
    |> (fun (x, y) ->
    List.filter
      (fun ele -> List.exists (fun (start, stop) -> ele >= start && ele <= stop) x)
      y)
    |> List.length
;;

let part_two () =
    Lazy.force parse_data
    |> fst
    |> List.sort (fun (start, _) (start2, _) -> compare start start2)
    |> valid_ids []
    |> List.fold_left (fun acc (start, stop) -> acc + abs (start - stop) + 1) 0
;;
