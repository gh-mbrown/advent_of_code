module Day02

open Utils

let private ParseInput () =
    lazy
        (readlines "inputs/day02.txt"
         |> List.map (fun x ->
             x.Split " "
             |> Array.filter (fun y -> y <> "")
             |> Array.map (function
                 | Int y -> y
                 | _ -> failwith "does not match pattern")
             |> List.ofArray))

let rec private IsDescending =
    function
    | []
    | [ _ ] -> true
    | x :: y :: rest -> x > y && IsDescending(y :: rest)

let rec private IsAscending =
    function
    | []
    | [ _ ] -> true
    | x :: y :: rest -> x < y && IsAscending(y :: rest)

let rec private DifferenceLessThanThree =
    function
    | []
    | [ _ ] -> true
    | x :: y :: rest ->
        let diff = abs (x - y)
        diff > 0 && diff < 4 && DifferenceLessThanThree(y :: rest)

let private IsGood =
    function
    | [] -> false
    | lst -> (IsDescending lst || IsAscending lst) && DifferenceLessThanThree lst

let private RemoveOneAndTryAgain =
    function
    | [] -> false
    | lst ->
        List.indexed lst
        |> List.exists (function
            | i, _ -> List.removeAt i lst |> IsGood)

let private PartOne () =
    ParseInput().Force() |> List.filter IsGood |> List.length

let private PartTwo () =
    ParseInput().Force()
    |> List.partition IsGood
    |> function
        | passed, failed -> List.length passed + (List.filter RemoveOneAndTryAgain failed |> List.length)


let Solve () =
    PartOne() |> Printf.printf "Day 2 Part 1: %d\n"
    PartTwo() |> Printf.printf "Day 2 Part 2: %d\n"
