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

let calcScenicScore row col value =
    if isOnEdge row col then
        0
    else
        let left =
            trees[row, *][0 .. col - 1]
            |> Array.rev
            |> Array.takeWhile (fun t -> t < value)
            |> Array.length
            |> zeroMeansOne

        let right =
            trees[row, *][col + 1 ..]
            |> Array.takeWhile (fun t -> t < value)
            |> Array.length
            |> zeroMeansOne

        left * right

// let right = trees[3, *][2 + 1 ..] |> Array.takeWhile (fun t -> t < 5)
trees |> Array2D.mapi calcScenicScore
