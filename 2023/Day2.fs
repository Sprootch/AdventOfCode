[<RequireQualifiedAccess>]
module Day2

open System.Text.RegularExpressions

type Color =
    | Blue
    | Red
    | Green

type Game = { Id: int; Draws: Draw array }
and Draw = { Color: Color; Number: int }

let toColor str =
    match str with
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | _ -> failwith "invalid color"

let regex(line: string) =
    let result = Regex.Match(line.Trim(), @"(?<number>\d+) (?<color>\w+)")
    let number = result.Groups["number"].Value |> int
    let color = result.Groups["color"].Value |> toColor
    { Color = color; Number = number }

let parse(line: string) = line.Split(',') |> Seq.map regex

let parseDraw(line: string) =
    line.Split(';') |> Seq.collect parse |> Seq.toArray

let parseGame(line: string) =
    let number = line[5] |> string |> int
    let draws = line[7..] |> parseDraw
    { Id = number; Draws = draws }

let validGame(game: Game) =
    game.Draws
    |> Array.exists (fun d ->
        (d.Color = Red && d.Number > 12)
        || (d.Color = Green && d.Number > 13)
        || (d.Color = Blue && d.Number > 14))
    |> not

let solve lines =
    lines |> Seq.map parseGame |> Seq.filter validGame |> Seq.sumBy (fun g -> g.Id)
