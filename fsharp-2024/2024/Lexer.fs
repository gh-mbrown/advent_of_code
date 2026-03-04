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
    | x when x < String.length input && System.Char.IsDigit input.[x] -> getDigit input (x + 1)
    | x -> x

let tokenize input =
    let rec aux pos tokens =
        match pos with
        | x when x < String.length input ->
            match input.[pos..] with
            | StartsWith "mul" _ -> aux (pos + 3) (Mul :: tokens)
            | StartsWith "(" _ -> aux (pos + 1) (LParen :: tokens)
            | StartsWith ")" _ -> aux (pos + 1) (RParen :: tokens)
            | StartsWith "," _ -> aux (pos + 1) (Comma :: tokens)
            | StartsWith "don't" _ -> aux (pos + 5) (Dont :: tokens)
            | StartsWith "do" _ -> aux (pos + 2) (Do :: tokens)
            | _ when System.Char.IsDigit input.[pos] ->
                getDigit input pos
                |> fun endPos ->
                    match input.[pos .. endPos - 1] with
                    | Int success -> aux endPos (Number success :: tokens)
                    | failed -> failwithf "not a number %s" failed
            | _ -> aux (pos + 1) (Other :: tokens)
        | _ -> List.rev tokens

    aux 0 []

let parse isPartTwo tokens =
    let rec aux enabled results =
        function
        | Dont :: LParen :: RParen :: rest when isPartTwo -> aux false results rest
        | Do :: LParen :: RParen :: rest when isPartTwo -> aux true results rest
        | Mul :: LParen :: Number x :: Comma :: Number y :: RParen :: rest ->
            match enabled with
            | true -> aux enabled ((x, y) :: results) rest
            | false -> aux enabled results rest
        | _ :: rest -> aux enabled results rest
        | [] -> results

    aux true [] tokens
