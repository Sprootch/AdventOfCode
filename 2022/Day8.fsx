open System.IO

let lines = File.ReadLines(Path.Combine("2022/Input", "day8.txt"))
let trees =
    array2D [ for line in lines -> [ for tree in line -> tree |> string |> int ] ]

printfn "%A" trees

let isVisibleFromLeftOrRight row col =
    let value = trees[row, col]
    trees[row,*][0..col-1] |> Array.forall (fun item -> item < value) || trees[row, *][col + 1 ..] |> Array.forall (fun item -> item < value)

let isVisible = isVisibleFromLeftOrRight 1 3

// columns
let x = [ for i in 0..4 -> trees[i, 0] ]

// [ for row in trees -> [ for col in row -> printfn "%A"col ] ]
// map |> List.iteri (fun i row -> printfn $"row %d{i} is %A{row}")

// let e = map |> List.length
// [1..e-2] |> List.iter (fun i -> printfn $"%A{map[i]}")
