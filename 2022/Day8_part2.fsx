open System.IO

let lines = File.ReadLines(Path.Combine("2022/Input", "day8_part2.txt"))

let trees =
    array2D [ for line in lines -> [ for tree in line -> tree |> string |> int ] ]

let isOnEdge row col =
    row = 0
    || col = 0
    || (row + 1) = (trees |> Array2D.length2)
    || (col + 1) = (trees |> Array2D.length1)

let zeroMeansOne value = if value = 0 then 1 else value

let getColumn idx =
    let j = trees |> Array2D.length1
    [| for i in 0 .. j - 1 -> trees[i, idx] |]

let myTakeWhile refValue state value =
    let contains = (state |> List.exists (fun item -> item >= refValue))
    if contains then state else value :: state

let calcScenicScore row col value =
    if isOnEdge row col then
        0
    else
        let myTakeWhile = myTakeWhile value

        let left =
            trees[row, *][0 .. col - 1]
            |> Array.rev
            |> Array.fold myTakeWhile list.Empty
            |> List.length
            |> zeroMeansOne

        let right =
            trees[row, *][col + 1 ..]
            |> Array.fold myTakeWhile list.Empty
            |> List.length
            |> zeroMeansOne

        let up =
            (getColumn col)[.. row - 1]
            |> Array.rev
            |> Array.fold myTakeWhile list.Empty
            |> List.length
            |> zeroMeansOne

        let down =
            (getColumn col)[row + 1 ..]
            |> Array.fold myTakeWhile list.Empty
            |> List.length
            |> zeroMeansOne

        up * left * right * down

trees |> Array2D.mapi calcScenicScore |> Seq.cast<int> |> Seq.max
