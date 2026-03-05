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
  { direction =
      (line.[0]
       |> function
       | x when x == 'L' -> LEFT
       | _ -> RIGHT)
  ; hundred =
      (match line with
       | x when String.length x > 3 -> Some (String.sub x 1 (String.length x - 3))
       | _ -> None)
  ; decimal =
      (match line with
       | x when String.length x > 3 -> String.sub x (String.length x - 2) 2
       | _ -> String.sub line 1 (String.length line - 1))
  }
;;

let parse_data = lazy (Common.File.read_file_list file_name |> List.map parse_line)

let add_to_combo combo = function
  | { direction; decimal; _ } when direction = LEFT -> combo - int_of_string decimal
  | { decimal; _ } -> combo + int_of_string decimal
;;

let add_to_count_one count combo =
  (match combo with
   | combo when combo > 99 -> combo - 100
   | combo when combo < 0 -> combo + 100
   | combo -> combo)
  |> function
  | x when x = 0 -> count + 1, x
  | x -> count, x
;;

let add_to_count_two count = function
  | combo when combo > 99 -> count + 1, combo - 100
  | combo when combo < 0 -> count + 1, combo + 100
  | combo -> count, combo
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
