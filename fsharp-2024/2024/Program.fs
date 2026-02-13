open Utils

let private ParseArgs =
    function
    | [||] -> failwith "no args provided"
    | [| Int day |] -> day
    | _ -> failwith "Usage: dotnet run <day> [part]"

let RunDay =
    function
    | 1 -> Day01.Solve()
    | 2 -> Day02.Solve()
    | _ -> failwith "invalid day"


[<EntryPoint>]
let Main args =
    ParseArgs args |> RunDay
    0
