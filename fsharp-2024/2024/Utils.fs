module Utils

let (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, value -> Some value
    | false, _ -> None

let readlines path =
    System.IO.File.ReadAllLines path |> Array.toList

let multiply a b = a * b
let add a b = a + b
