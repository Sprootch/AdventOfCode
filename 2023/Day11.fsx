type Space =
    | Empty
    | Galaxy

type Universe = Space array2d

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

let toSpace =
    function
    | '.' -> Empty
    | '#' -> Galaxy
    | _ -> failwith "Invalid space"

let lines input =
    [ for row, line in (input |> Seq.indexed) do
          [ for col, char in line |> Seq.indexed -> ((row, col), char |> toSpace) ] ]
    |> List.collect id

let mkUniverse input =
    let universe = Array2D.zeroCreate 10 10
    
    for row, line in (input |> Seq.indexed) do
        for col, char in line |> Seq.indexed do
            (char |> toSpace) |> Array2D.set universe row col

    universe

let isEmpty = Array.forall (fun s -> s = Empty)

let expand universe : Universe =
    let colCount = universe |> Array2D.length1
    let rowCount = universe |> Array2D.length2

    // create col row value
    // todo: get index of empty rows & cols
    let newUniverse = Array2D.create 13 12 Empty
    //
    // for row in 0..rowCount do
    //     let isEmpty = universe[row, *] |> Array.forall (fun s -> s = Empty)
    //     ()
        
// let x =
//     [ for i in 0 .. (Array2D.length2 universe - 1) -> if (isEmpty universe[i, *]) then Some i else None ]
//     |> List.choose id
//     |> List.mapi (+)
//
// let y =
//     [ for i in 0 .. (Array2D.length1 universe - 1) -> if (isEmpty universe[*, i]) then Some i else None ]
//     |> List.choose id
//     |> List.mapi (+)
    newUniverse 

let universe = mkUniverse example |> expand

let rowCount = universe |> Array2D.length2
let colCount = universe |> Array2D.length1

// for row in 0..9 do
//     let isEmpty = universe[row, *] |> Array.forall (fun s -> s = Empty)
//     printfn $"""Row %d{row} %A{if isEmpty then "empty" else "not empty"}"""
//
// for col in 0..9 do
//     let isEmpty = universe[*, col] |> Array.forall (fun s -> s = Empty)
//     printfn $"""Col %d{col} %A{if isEmpty then "empty" else "not empty"}"""
