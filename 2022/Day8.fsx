open System.IO

let txt = File.ReadLines(Path.Combine("2022/Input", "day8.txt"))
// let trees = [| for line in txt -> [| for tree in line -> tree |> string |> int |] |]
let trees = array2D [| for line in txt -> [| for tree in line -> tree |> string |> int |] |]
printfn "%A" trees

trees[1,*]

// [ for row in trees -> [ for col in row -> printfn "%A"col ] ]
// map |> List.iteri (fun i row -> printfn $"row %d{i} is %A{row}")

// let e = map |> List.length
// [1..e-2] |> List.iter (fun i -> printfn $"%A{map[i]}")
