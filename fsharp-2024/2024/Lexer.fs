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
    let rec aux tokens =
        function
        | pos when pos < String.length input ->
            match input.[pos..] with
            | StartsWith "mul" _ -> aux (Mul :: tokens) (pos + 3)
            | StartsWith "(" _ -> aux (LParen :: tokens) (pos + 1)
            | StartsWith ")" _ -> aux (RParen :: tokens) (pos + 1)
            | StartsWith "," _ -> aux (Comma :: tokens) (pos + 1)
            | StartsWith "don't" _ -> aux (Dont :: tokens) (pos + 5)
            | StartsWith "do" _ -> aux (Do :: tokens) (pos + 2)
            | _ when System.Char.IsDigit input.[pos] ->
                getDigit input pos
                |> fun endPos ->
                    match input.[pos .. endPos - 1] with
                    | Int success -> aux (Number success :: tokens) endPos
                    | failed -> failwithf "not a number %s" failed
            | _ -> aux (Other :: tokens) (pos + 1)
        | _ -> List.rev tokens

    aux [] 0

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
