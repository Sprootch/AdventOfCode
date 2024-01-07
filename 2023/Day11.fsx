type Space =
    | Empty
    | Galaxy

let example =
    """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""
        .Split("\n")
    |> Array.map (_.Trim())
    |> List.ofSeq

let parse =
    function
    | '.' -> Empty
    | '#' -> Galaxy
    | _ -> failwith "Invalid space"

let lines input =
    [ for row, line in (input |> Seq.indexed) do
          [ for col, char in line |> Seq.indexed -> ((row, col), char |> parse) ] ]
    |> List.collect id

let mkUniverse input array =
    for row, line in (input |> Seq.indexed) do
        for col, char in line |> Seq.indexed do
            Array2D.set array row col (char |> parse)

    array

let universe = Array2D.zeroCreate 10 10 |> mkUniverse example

let isEmpty = Array.forall (fun s -> s = Empty)

let x =
    [ for i in 0 .. (Array2D.length2 universe - 1) -> if (isEmpty universe[i, *]) then Some i else None ]
    |> List.choose id
    |> List.mapi (+)

let y =
    [ for i in 0 .. (Array2D.length1 universe - 1) -> if (isEmpty universe[*, i]) then Some i else None ]
    |> List.choose id
    |> List.mapi (+)
    
// let space = lines example

// for row in 0..9 do
//     let isEmpty =
//         space
//         |> List.filter (fun ((x, _), _) -> x = row)
//         |> List.forall (fun (_, space) -> space = Empty)
//
//     printfn $"""Row %d{row} %A{if isEmpty then "empty" else "not empty"}"""
//
// for col in 0..9 do
//     let isEmpty =
//         space
//         |> List.filter (fun ((_, y), _) -> y = col)
//         |> List.forall (fun (_, space) -> space = Empty)
//
//     printfn $"""Col %d{col} %A{if isEmpty then "empty" else "not empty"}"""
