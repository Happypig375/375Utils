open System

let read = Console.Read
let readln = Console.ReadLine
let readlns() =
     fun _ -> Console.ReadLine()
     |>  Seq.initInfinite
     |>  Seq.takeWhile ((<>) null)

[<EntryPoint>]
let rec main argv =
    printfn
        "\
Welcome to 375Utils, select one option to continue:
0) Exit
1) Generate Braille Patterns, result is saved to clipboard"
    let rec getInput() =
        match readln() |> Int32.TryParse with
        | true, x when x >= 0 -> x
        | _ -> printfn "Invalid input. Please input a valid positive integer: "
               getInput()
    let rec work x =
        match x with
        | 1 ->
            let rec get01() =
                match readln() |> Int32.TryParse with
                | true, 00 -> [0; 0]
                | true, 01 -> [0; 1]
                | true, 10 -> [1; 0]
                | true, 11 -> [1; 1]
                | _ -> printfn "Invalid input. Please input 00, 01, 10 or 11: "
                       get01()
            printfn "Please input 4 lines of 00, 01, 10 or 11: "
            [get01(); get01(); get01(); get01()]
            |> List.concat
            |> List.mapi (fun i x -> (match i with 1 -> 3 | 2 -> 1 | 3 -> 4 | 4 -> 2 | x -> x), x)
            |> List.sumBy (fun (i, x) -> x * pown 2 i)
            |> (+) 0x2800
            |> char
            |> string
            |> TextCopy.Clipboard.SetText
        | _ -> printfn "No matching option found. Please input a valid option: "
               getInput() |> work
    match getInput() with
    | 0 -> 0
    | x -> x |> work
           printfn "Option finished executing."
           main argv