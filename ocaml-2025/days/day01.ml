type direction =
    | LEFT
    | RIGHT

type combination =
    { direction : direction
    ; hundred : string option
    ; decimal : string
    }

let file_name = "input/day01.txt"

let parse_line line =
    { direction = (line.[0] |> fun x -> if x == 'L' then LEFT else RIGHT)
    ; hundred =
        (if String.length line > 3
         then Some (String.sub line 1 (String.length line - 3))
         else None)
    ; decimal =
        (if String.length line > 3
         then String.sub line (String.length line - 2) 2
         else String.sub line 1 (String.length line - 1))
    }
;;

let parse_data = lazy (Common.File.read_file_list file_name |> List.map parse_line)

let add_to_combo combo { direction; decimal; _ } =
    if direction = LEFT
    then combo - int_of_string decimal
    else combo + int_of_string decimal
;;

let add_to_count_one count combo =
    (if combo > 99 then combo - 100 else if combo < 0 then combo + 100 else combo)
    |> fun x -> if x = 0 then count + 1, x else count, x
;;

let add_to_count_two count combo =
    if combo > 99
    then count + 1, combo - 100
    else if combo < 0
    then count + 1, combo + 100
    else count, combo
;;

let add_hundred_to_count { hundred; _ } (count, combo) =
    match hundred with
    | Some value -> count + int_of_string value, combo
    | None -> count, combo
;;

let part_one () =
    Lazy.force parse_data
    |> List.fold_left
         (fun (count, combo) x -> add_to_combo combo x |> add_to_count_one count)
         (0, 50)
    |> fst
;;

let part_two () =
    Lazy.force parse_data
    |> List.fold_left
         (fun (count, combo) x ->
            add_to_combo combo x |> add_to_count_two count |> add_hundred_to_count x)
         (0, 50)
    |> fst
;;
