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

let expand universe =
    let isEmpty = Array.forall (fun s -> s = Empty)
    
    let rows = Array2D.length2 universe
    let cols = Array2D.length1 universe
    
    let emptyRowIndexes =
        [ for i in 0 .. (rows - 1) -> if (isEmpty universe[i, *]) then Some i else None ]
        |> List.choose id
        |> List.mapi (+)

    let emptyColIndexes =
        [ for i in 0 .. (cols - 1) -> if (isEmpty universe[*, i]) then Some i else None ]
        |> List.choose id
        |> List.mapi (+)

    let newRowCount = rows + emptyRowIndexes.Length
    let newColCount = cols + emptyColIndexes.Length
    
    let expandedUniverse = Array2D.create newColCount newRowCount Empty
    
    for i in 0..newRowCount-1 do
        if (emptyRowIndexes |> List.contains i) then printfn "Empty"
        else printfn "%A" expandedUniverse[i, *]
    // for row, line in (input |> Seq.indexed) do
    //     for col, char in line |> Seq.indexed do
    //         Array2D.set array row col (char |> parse)
            
    // for i in 0..(Array2D.)
    expandedUniverse

let mkUniverse input =
    let universe = Array2D.zeroCreate 10 10
    
    for row, line in (input |> Seq.indexed) do
        for col, char in line |> Seq.indexed do
            (char |> toSpace) |> Array2D.set universe row col

    universe

let isEmpty = Array.forall (fun s -> s = Empty)

let universe = mkUniverse example

let rows = Array2D.length2 universe
let cols = Array2D.length1 universe

let emptyRowIndexes =
    [ for i in 0 .. (rows - 1) -> if (isEmpty universe[i, *]) then Some i else None ]
    |> List.choose id
    |> List.mapi (+)

let emptyColIndexes =
    [ for i in 0 .. (cols - 1) -> if (isEmpty universe[*, i]) then Some i else None ]
    |> List.choose id
    |> List.mapi (+)

let newRowCount = rows + emptyRowIndexes.Length
let newColCount = cols + emptyColIndexes.Length

let expandedUniverse = Array2D.create newColCount newRowCount Empty

// for i in 0..newRowCount-1 do
//     if (emptyRowIndexes |> List.contains i) then printfn "Empty Row !"
//     else printfn "%A" expandedUniverse[i, *]
    

