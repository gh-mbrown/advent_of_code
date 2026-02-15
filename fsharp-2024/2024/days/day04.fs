module Day04

open Utils

type Postion = { row: int; col: int; ch: char }


let private parseInput () =
    lazy
        (readLines "inputs/day04.txt"
         |> List.mapi (fun row str -> Seq.toList str |> List.mapi (fun col ch -> (row, col), ch))
         |> List.concat)

let private hor_left (row, col) =
    [ row, col; row, col - 1; row, col - 2; row, col - 3 ]

let partOne () =
    parseInput().Force() |> fun x -> Map.ofList x

let solve () = "test"
