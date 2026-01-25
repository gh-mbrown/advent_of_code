let file_name = "input/day05.txt"

let parse_data =
    lazy
      (let rec aux range ids lst =
           match lst with
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

let rec valid_ids ids lst =
    match lst with
    | [] -> ids
    | (start, stop) :: xs ->
        (match ids with
         | [] -> valid_ids [ start, stop ] xs
         | (start2, stop2) :: ys ->
             if start <= stop2 + 1
             then valid_ids ((min start start2, max stop stop2) :: ys) xs
             else valid_ids ((start, stop) :: ids) xs)
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
