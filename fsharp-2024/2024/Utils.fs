module Utils

let (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, value -> Some value
    | false, _ -> None

let (|Double|_|) (str: string) =
    match System.Double.TryParse str with
    | true, value -> Some value
    | false, _ -> None

let (|StartsWith|_|) (prefix: string) =
    function
    | (str: string) when str.StartsWith prefix -> Some str
    | _ -> None

let readLines path =
    System.IO.File.ReadAllLines path |> List.ofArray

let multiply left right = left * right
let add left right = left + right
