module Day3

open System

let solve input = 0

let example =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
        .Split("\n")
    |> Array.map (_.Trim())
    |> List.ofSeq

type State =
    | Symbol of string
    | Digit of int
    | Empty

type Position = int * int

type Cell =
    | Position of Position
    | State of State
    
let parse =
    function
    | c when c |> Char.IsDigit -> Digit(c |> string |> int)
    | c when c = '.' -> Empty
    | c -> Symbol(c |> string)

let lines =
    [ for row, line in (example |> Seq.indexed) do
          [ for col, char in line |> Seq.indexed -> ((col, row), char |> parse) ] ]

let line = [((0, 0), Digit 4); ((1, 0), Digit 6); ((2, 0), Digit 7); ((3, 0), Empty);
    ((4, 0), Empty); ((5, 0), Digit 1); ((6, 0), Digit 1); ((7, 0), Digit 4);
    ((8, 0), Empty); ((9, 0), Empty)]

line |> List.takeWhile (fun (_, state) -> match state with | Digit _ -> true | _ -> false)