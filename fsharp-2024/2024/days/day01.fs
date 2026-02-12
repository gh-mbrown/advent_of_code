module Day01

open Utils

let parse_input () =
    lazy
        (readlines "inputs/day01.txt"
         |> List.map (fun x ->
             x.Split " "
             |> Array.filter (fun y -> y <> "")
             |> function
                 | [||] -> failwith "Array is empty"
                 | [| Int left; Int right |] -> left, right
                 | _ -> failwith "does not match pattern")
         |> List.unzip)

let part_one () =
    parse_input().Force()
    |> function
        | left, right -> List.sort left, List.sort right
    |> function
        | left, right -> List.fold2 (fun acc l r -> acc + abs (l - r)) 0 left right

let part_two () =
    parse_input().Force()
    |> function
        | left, right ->
            List.fold (fun acc x -> List.filter (fun y -> y = x) right |> List.length |> multiply x |> add acc) 0 left

let solve () =
    part_one () |> Printf.printf "Day 1 Part 1: %d\n"
    part_two () |> Printf.printf "Day 1 Part 2: %d\n"
