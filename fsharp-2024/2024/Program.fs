open Utils

let private parseArgs =
    function
    | [||] -> failwith "no args provided"
    | [| Int day |] -> day
    | _ -> failwith "Usage: dotnet run <day> [part]"

let runDay =
    function
    | 1 -> Day01.Solve()
    | 2 -> Day02.solve ()
    | _ -> failwith "invalid day"


[<EntryPoint>]
let main args =
    parseArgs args |> runDay
    0
