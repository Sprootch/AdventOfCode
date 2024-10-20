open System
open System.IO

type BingoCard = (int * bool) array2d

let lines = File.ReadLines(Path.Combine("2021/Input", "day4.txt"))

let draw = lines |> Seq.head |> (_.Split(',')) |> Array.map int

let mkDraw value (bingoCard: BingoCard) : BingoCard =
    bingoCard
    |> Array2D.map (fun tuple -> if fst tuple = value then (value, true) else tuple)

let hasWon bingoCard =
    let rowWon =
        let nbRows = bingoCard |> Array2D.length2

        [| for i in 0 .. (nbRows - 1) -> bingoCard[i, *] |> Array.forall (fun x -> snd x = true) |]
        |> Array.fold (fun a b -> a || b) false

    let colWon =
        let nbCols = bingoCard |> Array2D.length1

        [| for i in 0 .. (nbCols - 1) -> bingoCard[*, i] |> Array.forall (fun x -> snd x = true) |]
        |> Array.fold (fun a b -> a || b) false

    rowWon || colWon

let bingoCards =
    let folder state line =
        if (String.IsNullOrWhiteSpace line) then
            let newBingoCard = list.Empty
            newBingoCard :: state
        else
            let bingoCard = state |> List.head
            state |> List.updateAt 0 (line :: bingoCard)

    let createBingoCard (card: string list) : BingoCard =
        array2D
            [ for line in (card |> List.rev) ->
                  [ for str in line.Split(' ', StringSplitOptions.RemoveEmptyEntries) -> (str |> int, false) ] ]

    lines |> Seq.skip 1 |> Seq.fold folder List.empty |> List.map createBingoCard

let rec runBingo draws bingoCards =
    let numberDrawn = draws |> Array.head
    let bingoCards = bingoCards |> List.map (mkDraw numberDrawn)

    match (bingoCards |> List.filter (fun card -> card |> hasWon |> not)) with
    | [] -> (numberDrawn, bingoCards |> List.head)
    | list -> runBingo (draws[1..]) list

let lastNumber, lastWinningCard = runBingo draw bingoCards

let sumUndrawn =
    lastWinningCard
    |> Seq.cast<int * bool>
    |> Seq.filter (fun v -> snd v = false)
    |> Seq.sumBy fst

lastNumber * sumUndrawn
