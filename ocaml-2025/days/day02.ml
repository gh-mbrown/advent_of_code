let file_name = "input/day02.txt"

let parse_line line =
    String.split_on_char ',' line
    |> List.map (fun x -> String.split_on_char '-' x |> List.map int_of_string)
;;

let parse_data =
    lazy (Common.File.read_file_list file_name |> List.map parse_line |> List.concat)
;;

let find_repeat func lst =
    match lst with
    | [ start; stop ] ->
        let rec aux repeat current =
            if current > stop
            then repeat
            else if func (string_of_int current)
            then aux (current :: repeat) (current + 1)
            else aux repeat (current + 1)
        in
        aux [] start
    | _ -> failwith "does not match pattern"
;;

let is_repeat_one num =
    let len = String.length num in
    if len mod 2 <> 0
    then false
    else (
      let half = len / 2 in
      let rec aux i =
          if i >= half
          then true
          else if num.[i] <> num.[i + half]
          then false
          else aux (i + 1)
      in
      aux 0)
;;

let is_repeat_two num =
    let len = String.length num in
    let rec aux i =
        if i > len / 2
        then false
        else if len mod i <> 0
        then aux (i + 1)
        else (
          let rec aux_two j =
              if j >= len
              then true
              else if num.[j] <> num.[j mod i]
              then false
              else aux_two (j + 1)
          in
          if aux_two 0 then true else aux (i + 1))
    in
    aux 1
;;

let calc func =
    Lazy.force parse_data
    |> List.fold_left (fun acc x -> find_repeat func x |> List.fold_left ( + ) acc) 0
;;

let part_one () = calc is_repeat_one
let part_two () = calc is_repeat_two
