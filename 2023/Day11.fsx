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

let solve =
    let space = lines example

    for row in 0..9 do
        let isEmpty =
            space
            |> List.filter (fun ((x, _), _) -> x = row)
            |> List.forall (fun (_, space) -> space = Empty)

        printfn $"""Row %d{row} %A{if isEmpty then "empty" else "not empty"}"""

    for col in 0..9 do
        let isEmpty =
            space
            |> List.filter (fun ((_, y), _) -> y = col)
            |> List.forall (fun (_, space) -> space = Empty)

        printfn $"""Col %d{col} %A{if isEmpty then "empty" else "not empty"}"""
