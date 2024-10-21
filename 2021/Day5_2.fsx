open System
open System.IO

let mkTuples (array: string array) =
    let mkTuple (str: string) =
        let splitted = str.Split(',', StringSplitOptions.RemoveEmptyEntries)
        (splitted[0] |> int, splitted[1] |> int)

    (array[0] |> mkTuple, array[1] |> mkTuple)

let expand ((xs, ys), (xe, ye)) =
    if xs = xe then
        (if ys > ye then [ ye..ys ] |> List.rev else [ ys..ye ])
        |> List.map (fun y -> (xs, y))
    else if ys = ye then
        (if xs > xe then [ xe..xs ] |> List.rev else [ xs..xe ])
        |> List.map (fun x -> (x, ys))
    else
        // TODO
        []

let apply (map: int array2d) (x, y) =
    map[y, x] <- map[y, x] + 1
    map

let vents =
    File.ReadLines(Path.Combine("2021/Input", "day5.txt"))
    |> Seq.map (fun line -> line.Split("->", StringSplitOptions.RemoveEmptyEntries) |> mkTuples)
    |> Seq.map expand
    |> Seq.collect id
    |> Seq.toList

let maxX = vents |> List.maxBy fst |> fst
let maxY = vents |> List.maxBy snd |> snd

vents
|> List.fold apply (Array2D.create (maxX + 1) (maxY + 1) 0)
|> Seq.cast<int>
|> Seq.toList
|> Seq.countBy (fun x -> x > 1)

let (xs, ys), (xe, ye) = ((5, 5), (8, 2))
xs - xe
ys - ye
[ for i in xs .. xe ->
    (i, 0) ]
