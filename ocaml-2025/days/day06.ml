let file_name = "input/day06.txt"
let read_file = lazy (Common.File.read_file_list file_name)

let transpose lst =
    match lst with
    | [] -> []
    | [] :: _ -> []
    | _ ->
        let rec aux acc = function
            | [] :: _ -> List.rev acc
            | rows ->
                let hds =
                    List.filter_map
                      (function
                        | [] -> None
                        | h :: _ -> Some h)
                      rows
                in
                let tls =
                    List.filter_map
                      (function
                        | [] -> None
                        | _ :: t -> Some t)
                      rows
                in
                aux (hds :: acc) tls
        in
        aux [] lst
;;

let split_lst lst =
    let rec aux lst acc n_lst =
        match lst with
        | [] -> n_lst :: acc
        | x :: xs ->
            let str_lst = List.init (String.length x) (String.get x) in
            List.find_opt (fun y -> y = '*' || y = '+') str_lst
            |> (function
             | Some ch ->
                 List.filter (fun y -> y <> ch && y <> ' ') str_lst
                 |> List.to_seq
                 |> String.of_seq
                 |> fun y -> aux xs acc (String.make 1 ch :: y :: n_lst)
             | None ->
                 String.trim x
                 |> fun y ->
                 if y = "" then aux xs (n_lst :: acc) [] else aux xs acc (y :: n_lst))
    in
    aux lst [] []
;;

let format_data_one lst =
    List.map (fun x -> String.split_on_char ' ' x |> List.filter (fun y -> y <> "")) lst
    |> transpose
;;

let format_data_two lst =
    List.map (fun x -> List.init (String.length x) (String.get x)) lst
    |> transpose
    |> List.map (fun x -> List.to_seq x |> String.of_seq)
    |> split_lst
;;

let calc op func start lst =
    List.fold_left
      (fun acc x ->
         List.filter_map (fun y -> if y = op then None else Some y) x
         |> fun y ->
         if List.length x <> List.length y
         then
           List.fold_left (fun acc2 z -> func acc2 (int_of_string z)) start y
           |> Int.add acc
         else acc)
      0
      lst
;;

let full_calc lst = calc "+" Int.add 0 lst + calc "*" Int.mul 1 lst
let part_one () = Lazy.force read_file |> format_data_one |> full_calc
let part_two () = Lazy.force read_file |> format_data_two |> full_calc
(*test*)
