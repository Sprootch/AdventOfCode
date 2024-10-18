open System.IO

type BingoCard =
    { Lines: string list
    }

let lines = File.ReadLines(Path.Combine("2021/Input", "day4.txt"))

let draw = lines |> Seq.head

lines |> Seq.skip 1
