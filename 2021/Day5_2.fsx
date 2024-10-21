open System
open System.IO

let mkTuples (array: string array) =
    let mkTuple (str: string) =
        let splitted = str.Split(',', StringSplitOptions.RemoveEmptyEntries)
        (splitted[0] |> int, splitted[1] |> int)

    (array[0] |> mkTuple, array[1] |> mkTuple)

let expand ((xs: int, ys: int), (xe, ye)) =
    let expandDiagonally() =
        let inc = if xs < xe then 1 else -1
        let inc2 = if ys < ye then 1 else -1

        let l1 = [ for i in 0 .. Math.Abs(xs - xe) -> i * inc ]
        let l2 = [ for i in 0 .. Math.Abs(ys - ye) -> i * inc2 ]
        List.zip l1 l2 |> List.map (fun (a, b) -> (xs + a, ys + b))
    if xs = xe then
        (if ys > ye then [ ye..ys ] |> List.rev else [ ys..ye ])
        |> List.map (fun y -> (xs, y))
    else if ys = ye then
        (if xs > xe then [ xe..xs ] |> List.rev else [ xs..xe ])
        |> List.map (fun x -> (x, ys))
    else
        expandDiagonally()

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
|> List.fold apply (Array2D.create (maxY + 1) (maxX + 1) 0)
|> Seq.cast<int>
|> Seq.countBy (fun x -> x > 1)
