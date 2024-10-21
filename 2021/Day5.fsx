open System
open System.IO

let mkTuples (array: string array) =
    let mkTuple (str: string) =
        let splitted = str.Split(',', StringSplitOptions.RemoveEmptyEntries)
        (splitted[0] |> int, splitted[1] |> int)

    (array[0] |> mkTuple, array[1] |> mkTuple)

let isVerticalOrHorizontal ((xs, ys), (xe, ye)) = xs = xe || ys = ye

let expand ((xs, ys), (xe, ye)) =
    if xs = xe then
        (if ys > ye then [ ye..ys ] |> List.rev else [ ys..ye ])
        |> List.map (fun y -> (xs, y))
    else
        (if xs > xe then [ xe..xs ] |> List.rev else [ xs..xe ])
        |> List.map (fun x -> (x, ys))

let apply (map:int array2d) (x, y) =
    map[x, y] <- map[x, y] + 1
    map
// map |> Array2D.mapi (fun j i v -> if i = x && j = y then v + 1 else v)

let j =
    File.ReadLines(Path.Combine("2021/Input", "day5.txt"))
    |> Seq.map (fun line -> line.Split("->", StringSplitOptions.RemoveEmptyEntries) |> mkTuples)
    |> Seq.filter isVerticalOrHorizontal
    |> Seq.map expand
    |> Seq.collect id
    |> Seq.toList

let maxX = j |> List.maxBy fst |> fst
let maxY = j |> List.maxBy snd |> snd

let map = Array2D.create (maxX + 1) (maxY + 1) 0

let newMap = j |> List.fold apply map
newMap |> Seq.cast<int> |> Seq.countBy (fun x -> x > 1)
