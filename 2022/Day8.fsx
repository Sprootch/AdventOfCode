open System.IO

let lines = File.ReadLines(Path.Combine("2022/Input", "day8.txt"))

let trees =
    array2D [ for line in lines -> [ for tree in line -> tree |> string |> int ] ]

let isVisibleFromLeftOrRight row col value =
    trees[row, *][0 .. col - 1] |> Array.forall (fun item -> item < value)
    || trees[row, *][col + 1 ..] |> Array.forall (fun item -> item < value)

let getColumn idx =
    let j = trees |> Array2D.length1
    [| for i in 0 .. j - 1 -> trees[i, idx] |]

let isVisibleFromUpOrDown row col value =
    let column = getColumn col

    column[0 .. row - 1] |> Array.forall (fun item -> item < value)
    || column[row + 1 ..] |> Array.forall (fun item -> item < value)

let isVisible row col value =
    isVisibleFromLeftOrRight row col value || isVisibleFromUpOrDown row col value

trees
|> Array2D.mapi isVisible
|> Seq.cast<bool>
|> Seq.filter (fun x -> x = true)
|> Seq.length
