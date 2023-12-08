module Day4

open System
open System.IO
open System.Linq
open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit

let countPoints(line: string) =
    let idx = line.IndexOf(':')
    let c = line[idx + 1 ..].Split('|')

    let winningNumbers = Regex.Split(c[0].Trim(), @"\s+")

    let myNumbers = Regex.Split(c[1].Trim(), @"\s+")

    let winners = myNumbers.Count(fun x -> winningNumbers.Contains(x))

    Math.Pow(2., (winners - 1) |> float) |> int

let solve(line: string seq) =
    line |> Seq.map countPoints |> Seq.toArray |> Array.sum

let example =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

[<Fact>]
let ``Part 1 (example)``() =
    let txt = example.Split(Environment.NewLine)

    let result = solve txt

    result |> should equal 13

[<Fact>]
let ``Part 1``() =
    let txt = File.ReadLines(Path.Combine("Input", "day4.txt"))

    let result = solve txt

    result |> should equal 18619
