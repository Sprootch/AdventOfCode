open System.IO

let txt = File.ReadLines(Path.Combine("2022/Input", "day8.txt"))
let map = [ for line in txt -> [ for tree in line -> tree |> string |> int ] ]

map[1][4]
