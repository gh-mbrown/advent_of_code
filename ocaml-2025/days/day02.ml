let file_name = "input/day02.txt"

let parse_line line =
    String.split_on_char ',' line
    |> List.map (fun x -> String.split_on_char '-' x |> List.map int_of_string)
;;

let parse_data =
    lazy (Common.File.read_file_list file_name |> List.map parse_line |> List.concat)
;;

let find_repeat func = function
    | [ start; stop ] ->
        let rec aux repeat = function
            | current when current > stop -> repeat
            | current when func (string_of_int current) ->
                aux (current :: repeat) (current + 1)
            | current -> aux repeat (current + 1)
        in
        aux [] start
    | _ -> failwith "does not match pattern"
;;

let is_repeat_one num =
    String.length num
    |> function
    | len when len mod 2 <> 0 -> false
    | len ->
        let half = len / 2 in
        let rec aux = function
            | i when i >= half -> true
            | i when num.[i] <> num.[i + half] -> false
            | i -> aux (i + 1)
        in
        aux 0
;;

let is_repeat_two num =
    String.length num
    |> fun len ->
    let rec aux = function
        | i when i > len / 2 -> false
        | i when len mod i <> 0 -> aux (i + 1)
        | i ->
            let rec aux_two = function
                | j when j >= len -> true
                | j when num.[j] <> num.[j mod i] -> false
                | j -> aux_two (j + 1)
            in
            (match aux_two 0 with
             | false -> aux (i + 1)
             | true -> true)
    in
    aux 1
;;

let calc func =
    Lazy.force parse_data
    |> List.fold_left (fun acc x -> find_repeat func x |> List.fold_left ( + ) acc) 0
;;

let part_one () = calc is_repeat_one
let part_two () = calc is_repeat_two
