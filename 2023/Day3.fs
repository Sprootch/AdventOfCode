module Day3

open System
open Xunit

let solve x = 0

let example =
    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..".Split(Environment.NewLine)

[<Fact>]
let ``Part 1 (example)``() =
    let result = solve example

    Assert.Equal(4361, result)
