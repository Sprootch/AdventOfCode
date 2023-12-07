module Day1

open System
open System.IO
open Xunit

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

let solve input =
    input |> Seq.map processLine |> Seq.sum
    
let solve2 input =
    input |> Seq.map replaceDigits |> Seq.map processLine |> Seq.sum
    
[<Fact>]
let ``Part 1 (example)``() =
    let example =
        "1abc2
             pqr3stu8vwx
             a1b2c3d4e5f
             treb7uchet"
            .Split(Environment.NewLine)

    let result = solve example
    Assert.Equal(142, result)

[<Fact>]
let ``Part 1``() =
    let txt = File.ReadLines(Path.Combine("Input", "day1.txt"))
    let result = solve txt
    Assert.Equal(55488, result)

[<Fact>]
let ``Part 2 (example)``() =
    let example =
        "two1nine
             eightwothree
             abcone2threexyz
             xtwone3four
             4nineeightseven2
             zoneight234
             7pqrstsixteen"
            .Split(Environment.NewLine)

    let result = solve2 example

    Assert.Equal(281, result)
