open System
open System.IO

type BingoCard = int array2d

let lines = File.ReadLines(Path.Combine("2021/Input", "day4.txt"))

let draw = lines |> Seq.head |> (_.Split(',')) |> Array.map int

let folder state line =
    if (String.IsNullOrWhiteSpace line) then
        let newBingoCard = list.Empty
        newBingoCard :: state
    else
        let bingoCard = state |> List.head
        state |> List.updateAt 0 (line :: bingoCard)

let createBingoCard (card: string list) : BingoCard =
    array2D
        [ for line in (card |> List.rev) ->
              [ for str in line.Split(' ', StringSplitOptions.RemoveEmptyEntries) -> str |> int ] ]

lines |> Seq.skip 1 |> Seq.fold folder List.empty |> List.map createBingoCard
