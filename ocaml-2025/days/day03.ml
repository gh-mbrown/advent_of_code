let file_name = "input/day03.txt"

let parse_data =
    lazy
      (Common.File.read_file_list file_name
       |> List.map (fun x -> List.init (String.length x) (String.get x)))
;;

let find_num len = function
    | [] -> []
    | x :: xs ->
        let rec aux prev = function
            | [] -> List.rev prev
            | first :: rest ->
                (match prev with
                 | p when List.length p < len -> aux (first :: prev) rest
                 | p ->
                     aux
                       (List.mapi (fun i _ -> first :: Common.Listext.remove_at_i i p) p
                        |> (fun z -> p :: z)
                        |> List.sort (fun x1 x2 -> compare (List.rev x2) (List.rev x1))
                        |> List.hd)
                       p)
        in
        aux [ x ] xs
;;

let calc len =
    Lazy.force parse_data
    |> List.map (find_num len)
    |> List.fold_left (fun acc x -> acc + int_of_string (String.of_seq (List.to_seq x))) 0
;;

let part_one () = calc 2
let part_two () = calc 12
