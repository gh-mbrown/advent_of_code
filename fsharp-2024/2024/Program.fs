open Utils

let private parseArgs =
    function
    | [||] -> failwith "no args provided"
    | [| Int day |] -> day
    | [| _; _ |] -> failwith "Usage: dotnet run <day>"
    | _ -> failwith "Input passed was not a int"

let runDay =
    function
    | 1 -> Day01.solve ()
    | 2 -> Day02.solve ()
    | 3 -> Day03.solve ()
    | 4 -> Day04.solve ()
    | _ -> failwith "invalid day"


[<EntryPoint>]
let main args =
    parseArgs args |> runDay
    0
