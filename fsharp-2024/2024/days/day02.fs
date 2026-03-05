module Day02

open Utils

let private parseInput () =
    lazy
        (readLines "inputs/day02.txt"
         |> List.map (fun x ->
             x.Split " "
             |> Array.filter (fun y -> y <> "")
             |> Array.map (function
                 | Int y -> y
                 | _ -> failwith "does not match pattern")
             |> List.ofArray))

let rec private isDescending =
    function
    | []
    | [ _ ] -> true
    | head :: next :: rest -> head > next && isDescending (next :: rest)

let rec private isAscending =
    function
    | []
    | [ _ ] -> true
    | head :: next :: rest -> head < next && isAscending (next :: rest)

let rec private differenceLessThanThree =
    function
    | []
    | [ _ ] -> true
    | head :: next :: rest ->
        abs (head - next)
        |> fun diff -> diff > 0 && diff < 4 && differenceLessThanThree (next :: rest)

let private isGood =
    function
    | [] -> false
    | lst -> (isDescending lst || isAscending lst) && differenceLessThanThree lst

let private removeOneAndTryAgain =
    function
    | [] -> false
    | lst -> List.indexed lst |> List.exists (fun (i, _) -> List.removeAt i lst |> isGood)

let private partOne () =
    parseInput().Force() |> List.filter isGood |> List.length

let private partTwo () =
    parseInput().Force()
    |> List.partition isGood
    |> fun (passed, failed) -> List.length passed + (List.filter removeOneAndTryAgain failed |> List.length)


let solve () =
    partOne () |> Printf.printf "Day 2 Part 1: %d\n"
    partTwo () |> Printf.printf "Day 2 Part 2: %d\n"
