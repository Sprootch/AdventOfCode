open System
open System.IO

type BingoCard = (int * bool) array2d

let lines = File.ReadLines(Path.Combine("2021/Input", "day4.txt"))

let draw = lines |> Seq.head |> (_.Split(',')) |> Array.map int

let folder state line =
    if (String.IsNullOrWhiteSpace line) then
        let newBingoCard = list.Empty
        newBingoCard :: state
    else
        let bingoCard = state |> List.head
        state |> List.updateAt 0 (line :: bingoCard)

let mkDraw value (bingoCard: BingoCard) : BingoCard =
    bingoCard
    |> Array2D.map (fun tuple -> if fst tuple = value then (value, true) else tuple)

let createBingoCard (card: string list) : BingoCard =
    array2D
        [ for line in (card |> List.rev) ->
              [ for str in line.Split(' ', StringSplitOptions.RemoveEmptyEntries) -> (str |> int, false) ] ]

let hasWon (bingoCard: BingoCard) : bool =
    // let x = bingoCard[0, 0]
    let x =
        [| for i in 0..4 -> bingoCard[i, 0] |]
        |> Array.forall (fun spot -> snd spot = true)

    false

let bingoCards =
    lines |> Seq.skip 1 |> Seq.fold folder List.empty |> List.map createBingoCard

let first = bingoCards |> List.head
let nbRows = 5 // todo
[| for i in 0 .. (nbRows - 1) -> first[i, *] |> Array.forall (fun x -> snd x = true) |] |> Array.fold (fun a b -> a || b) false
[|false; false; true; false; false|]|> Array.fold (fun a b -> a || b) false
first[0, *] // first row
first[*, 0] // first column

let mkDraws bingoCards value =
    let bingoCards = bingoCards |> List.map (mkDraw value)
    if (bingoCards |> List.exists hasWon) then [] else []


draw |> Array.fold mkDraws bingoCards
