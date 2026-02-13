module Utils

let (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, value -> Some value
    | false, _ -> None

let readLines path =
    System.IO.File.ReadAllLines path |> List.ofArray

let multiply a b = a * b
let add a b = a + b
