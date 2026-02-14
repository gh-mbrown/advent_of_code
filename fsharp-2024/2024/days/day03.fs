module Day03

open Utils
open Lexer

let private parseInput () =
    lazy (readLines "inputs/day03.txt" |> String.concat "" |> tokenize)

let private solution isPartTwo =
    parseInput().Force()
    |> parse isPartTwo
    |> List.fold (fun acc (x, y) -> acc + x * y) 0

let solve () =
    solution false |> Printf.printf "Day 3 Part 1: %d\n"
    solution true |> Printf.printf "Day 3 Part 2: %d\n"
