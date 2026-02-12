open Utils

let parse_args args =
    match args with
    | [||] -> failwith "no args provided"
    | [| Int day |] -> day
    | _ -> failwith "Usage: dotnet run <day> [part]"

let run_day =
    function
    | 1 -> Day01.solve ()
    | _ -> failwith "invalid day"


[<EntryPoint>]
let main args =
    parse_args args |> run_day
    0
