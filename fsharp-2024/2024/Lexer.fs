module Lexer

open Utils

type Token =
    | Mul
    | LParen
    | RParen
    | Comma
    | Number of int
    | Dont
    | Do
    | Other

let rec private getDigit input =
    function
    | pos when pos < String.length input && System.Char.IsDigit input.[pos] -> getDigit input (pos + 1)
    | pos -> pos

let tokenize input =
    let rec aux pos tokens =
        match pos with
        | x when x < String.length input ->
            match input.[x..] with
            | StartsWith "mul" _ -> aux (x + 3) (Mul :: tokens)
            | StartsWith "(" _ -> aux (x + 1) (LParen :: tokens)
            | StartsWith ")" _ -> aux (x + 1) (RParen :: tokens)
            | StartsWith "," _ -> aux (x + 1) (Comma :: tokens)
            | StartsWith "don't" _ -> aux (x + 5) (Dont :: tokens)
            | StartsWith "do" _ -> aux (x + 2) (Do :: tokens)
            | _ when System.Char.IsDigit input.[x] ->
                getDigit input x
                |> fun endPos ->
                    match input.[x .. endPos - 1] with
                    | Int success -> aux endPos (Number success :: tokens)
                    | failed -> failwithf "not a number %s" failed
            | _ -> aux (x + 1) (Other :: tokens)
        | _ -> List.rev tokens

    aux 0 []

let parse isPartTwo tokens =
    let rec aux enabled results =
        function
        | Dont :: LParen :: RParen :: rest when isPartTwo -> aux false results rest
        | Do :: LParen :: RParen :: rest when isPartTwo -> aux true results rest
        | Mul :: LParen :: Number x :: Comma :: Number y :: RParen :: rest ->
            match enabled with
            | true -> aux true ((x, y) :: results) rest
            | false -> aux false results rest
        | _ :: rest -> aux enabled results rest
        | [] -> results

    aux true [] tokens
