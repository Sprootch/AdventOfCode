module Day4

open System
open System.IO
open System.Linq
open Xunit

let countPoints(line: string) =
    let idx = line.IndexOf(':')
    let c = line[idx + 1 ..].Split('|')

    let winningNumbers =
        c[0].Trim().Split(' ')
        |> Array.filter (fun c -> String.IsNullOrWhiteSpace c |> not)

    let myNumbers =
        c[1].Trim().Split(' ')
        |> Array.filter (fun c -> String.IsNullOrWhiteSpace c |> not)

    let res = winningNumbers.Intersect myNumbers

    match res.Count() with
    | n when n <= 2 -> n
    | n -> n * 2


let solve(line: string seq) =
    let c = line |> Seq.map countPoints |> Seq.toArray
    printfn "%A" c
    c |> Array.sum

let example =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11."

[<Fact>]
let ``Part 1 (example)``() =
    let c = example.Split(Environment.NewLine)

    let result = solve c

    Assert.Equal(13, result)

[<Fact>]
let ``Part 1``() =
    let txt = File.ReadLines(Path.Combine("Input", "day4.txt"))

    let result = solve txt

    result
// Assert.gr(1777, result)
