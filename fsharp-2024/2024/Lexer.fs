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

let rec private getDigit pos input =
    if pos < String.length input && System.Char.IsDigit input.[pos] then
        getDigit (pos + 1) input
    else
        pos

let tokenize input =
    let rec aux pos tokens =
        if pos >= String.length input then
            List.rev tokens
        else
            input.[pos..]
            |> function
                | StartsWith "mul" _ -> aux (pos + 3) (Mul :: tokens)
                | StartsWith "(" _ -> aux (pos + 1) (LParen :: tokens)
                | StartsWith ")" _ -> aux (pos + 1) (RParen :: tokens)
                | StartsWith "," _ -> aux (pos + 1) (Comma :: tokens)
                | StartsWith "don't" _ -> aux (pos + 5) (Dont :: tokens)
                | StartsWith "do" _ -> aux (pos + 2) (Do :: tokens)
                | _ when System.Char.IsDigit input.[pos] ->
                    getDigit pos input
                    |> fun x ->
                        input.[pos .. x - 1]
                        |> function
                            | Int y -> aux x (Number y :: tokens)
                            | y -> failwithf "not a number %s" y
                | _ -> aux (pos + 1) (Other :: tokens)

    aux 0 []

let parse isPartTwo tokens =
    let rec aux enabled results =
        function
        | Dont :: LParen :: RParen :: rest when isPartTwo -> aux false results rest
        | Do :: LParen :: RParen :: rest when isPartTwo -> aux true results rest
        | Mul :: LParen :: Number x :: Comma :: Number y :: RParen :: rest ->
            if enabled then
                aux enabled ((x, y) :: results) rest
            else
                aux enabled results rest
        | _ :: rest -> aux enabled results rest
        | [] -> results

    aux true [] tokens
