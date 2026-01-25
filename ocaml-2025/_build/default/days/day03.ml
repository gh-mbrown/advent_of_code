let file_name = "input/day03.txt"

let parse_data =
    lazy
      (Common.File.read_file_list file_name
       |> List.map (fun x -> List.init (String.length x) (String.get x)))
;;

let find_num len lst =
    match lst with
    | [] -> lst
    | x :: xs ->
        let rec aux next prev =
            match next with
            | [] -> List.rev prev
            | y :: ys ->
                if List.length prev < len
                then aux ys (y :: prev)
                else
                  List.mapi (fun i _ -> y :: Common.Listext.remove_at_i i prev) prev
                  |> (fun x -> prev :: x)
                  |> List.sort (fun x1 x2 -> compare (List.rev x2) (List.rev x1))
                  |> List.hd
                  |> aux ys
        in
        aux xs [ x ]
;;

let calc len =
    Lazy.force parse_data
    |> List.map (find_num len)
    |> List.fold_left (fun acc x -> acc + int_of_string (String.of_seq (List.to_seq x))) 0
;;

let part_one () = calc 2
let part_two () = calc 12
