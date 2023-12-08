module Day4

open System
open System.IO
open System.Linq
open System.Text.RegularExpressions
open Xunit
open Xunit.Abstractions

let countPoints (line: string) (helper: ITestOutputHelper) =
    let idx = line.IndexOf(':')
    let c = line[idx + 1 ..].Split('|')

    let winningNumbers = Regex.Split(c[0].Trim(), @"\s+")

    let myNumbers = Regex.Split(c[1].Trim(), @"\s+")

    let res = winningNumbers.Intersect myNumbers

    let result =
        match res.Count() with
        | n when n <= 2 -> n
        | n -> n * 2

    helper.WriteLine(result.ToString() + ": " + String.Join(", ", res.ToList()))
    
    result

let solve (line: string seq) (helper: ITestOutputHelper) =
    let c = line |> Seq.map (fun c -> countPoints c helper) |> Seq.toArray
    helper.WriteLine(String.Join(", ", c))
    // c |> Array.iter (fun c -> helper.WriteLine (c.ToString()))
    c |> Array.sum

let example =
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

type x(helper: ITestOutputHelper) =
    [<Fact>]
    let ``Part 1 (example)``() =
        let c = example.Split(Environment.NewLine)

        let result = solve c helper

        Assert.Equal(13, result)
        // result |> should equal 13

    [<Fact>]
    let ``Part 1``() =
        let txt = File.ReadLines(Path.Combine("Input", "day4.txt"))

        let result = solve txt helper

        result
        // should be greater than 5682
// result |> should not' (equal 1777)
// result |> should not' (equal 1581)
