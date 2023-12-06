module Day1

open System

module Seq =
    let tap seq =
        seq
        |> Seq.map (fun s ->
            printfn "%A" s
            s)

let processLine(str: string) =
    let digits = str |> Seq.filter Char.IsDigit
    let first = digits |> Seq.head
    let last = digits |> Seq.last
    Convert.ToInt32($"{first}{last}")

let replaceDigits(line: string) =
    line
        .Replace("eight", "8")
        .Replace("two", "2")
        .Replace("one", "1")
        .Replace("three", "3")
        .Replace("four", "4")
        .Replace("five", "5")
        .Replace("six", "6")
        .Replace("seven", "7")
        .Replace("nine", "9")

let count input =
    input |> Seq.map processLine |> Seq.sum
    
let count2 input =
    input |> Seq.map replaceDigits |> Seq.map processLine |> Seq.sum
