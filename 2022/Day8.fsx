open System.IO

let txt = File.ReadLines(Path.Combine("2022/Input", "day8.txt"))
// let trees = [| for line in txt -> [| for tree in line -> tree |> string |> int |] |]
let trees =
    array2D [ for line in txt -> [ for tree in line -> tree |> string |> int ] ]

printfn "%A" trees

let isVisible row col treeHeight =
    let allExceptMe = trees[row,*]|> List.filter (fun )//TODO

    allExceptMe |> List.forall (fun height -> height < treeHeight)

trees |> Array2D.mapi (fun row col value -> $"({row}, {col} = {value}")
// columns
let x = [ for i in 0..4 -> trees[i, 0] ]

// [ for row in trees -> [ for col in row -> printfn "%A"col ] ]
// map |> List.iteri (fun i row -> printfn $"row %d{i} is %A{row}")

// let e = map |> List.length
// [1..e-2] |> List.iter (fun i -> printfn $"%A{map[i]}")
