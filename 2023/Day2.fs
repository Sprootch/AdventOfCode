[<RequireQualifiedAccess>]
module Day2

open System.Text.RegularExpressions

type Color =
    | Blue
    | Red
    | Green

type Game = { Id: int; Draws: Draw array }
and Draw = { Color: Color; Number: int }

let toColor =
    function
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | _ -> failwith "invalid color"

let regex(line: string) =
    let result = Regex.Match(line.Trim(), @"(?<number>\d+) (?<color>\w+)")
    let number = result.Groups["number"].Value |> int
    let color = result.Groups["color"].Value |> toColor
    { Color = color; Number = number }

let parseDraw(line: string) =
    line.Split(';')
    |> Seq.collect (fun line -> line.Split(',') |> Seq.map regex)
    |> Seq.toArray

let parseGame(line: string) =
    let result = Regex.Match(line.Trim(), @"Game (?<number>\d+): (?<rest>)")
    let number = result.Groups["number"].Value |> int
    let idx = line.IndexOf(':') + 1
    let draws = line[idx..] |> parseDraw
    { Id = number; Draws = draws }

let validGame(game: Game) =
    game.Draws
    |> Array.exists (fun d ->
        (d.Color = Red && d.Number > 12)
        || (d.Color = Green && d.Number > 13)
        || (d.Color = Blue && d.Number > 14))
    |> not

let maxColor(line: Game) =
    let max color =
        line.Draws
        |> Array.filter (fun d -> d.Color = color)
        |> Array.maxBy (fun d -> d.Number)
        |> fun d -> d.Number

    [ max Red; max Green; max Blue ] |> List.reduce (*)

let solve lines =
    lines |> Seq.map parseGame |> Seq.filter validGame |> Seq.sumBy (fun g -> g.Id)

let solve2 lines =
    lines |> Seq.map parseGame |> Seq.map maxColor |> Seq.sum
