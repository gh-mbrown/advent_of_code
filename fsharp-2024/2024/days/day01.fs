module Day01

open Utils

let private parseInput () =
    lazy
        (readLines "inputs/day01.txt"
         |> List.map (fun x ->
             x.Split " "
             |> Array.filter (fun y -> y <> "")
             |> function
                 | [||] -> failwith "Array is empty"
                 | [| _ |] -> failwith "Only one element"
                 | [| Int left; Int right |] -> left, right
                 | _ -> failwith "does not match pattern")
         |> List.unzip)

let private partOne () =
    parseInput().Force()
    |> fun (left, right) -> List.sort left, List.sort right
    |> fun (left, right) -> List.fold2 (fun acc l r -> acc + abs (l - r)) 0 left right

let private partTwo () =
    parseInput().Force()
    |> fun (left, right) ->
        List.fold
            (fun acc x ->
                List.filter (fun y -> y = x) right
                |> List.length
                |> fun z -> z * x |> fun a -> a + acc)
            0
            left

let solve () =
    partOne () |> Printf.printf "Day 1 Part 1: %d\n"
    partTwo () |> Printf.printf "Day 1 Part 2: %d\n"
