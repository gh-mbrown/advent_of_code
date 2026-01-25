let file_name = "input/day06.txt"
let read_file = lazy (Common.File.read_file_list file_name)

let rec transpose = function
    | [] -> []
    | x ->
        List.filter (fun y -> y <> []) x
        |> (function
         | [] -> []
         | rows -> List.map List.hd rows :: transpose (List.map List.tl rows))
;;

let remove_op op lst =
    let removed = ref false in
    let new_lst =
        List.map
          (fun x ->
             if String.contains x op
             then (
               removed := true;
               String.to_seq x |> Seq.filter (fun y -> y <> op) |> String.of_seq)
             else x)
          lst
    in
    if !removed then String.make 1 op :: new_lst else new_lst
;;

let format_data_one lst =
    List.map (fun x -> String.split_on_char ' ' x |> List.filter (fun y -> y <> "")) lst
    |> transpose
;;

let format_data_two lst =
    let l =
        format_data_one lst
        |> List.map (fun x ->
          List.map (fun y -> String.to_seq y |> List.of_seq) x
          |> transpose
          |> List.map (fun y -> List.to_seq y |> String.of_seq)
          |> remove_op '+'
          |> remove_op '*')
    in
    List.iter Common.Listext.print_string_list l;
    l
;;

let calc op func start lst =
    List.fold_left
      (fun acc x ->
         List.find_index (fun y -> y = op) x
         |> function
         | Some idx ->
             Common.Listext.remove_at_i idx x
             |> List.fold_left (fun acc2 z -> func acc2 (int_of_string z)) start
             |> Int.add acc
         | None -> acc)
      0
      lst
;;

let full_calc lst = calc "+" Int.add 0 lst + calc "*" Int.mul 1 lst
let part_one () = Lazy.force read_file |> format_data_one |> full_calc
let part_two () = Lazy.force read_file |> format_data_two |> full_calc
