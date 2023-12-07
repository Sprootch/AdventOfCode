module Day2

open System
open System.IO
open System.Text.RegularExpressions
open Xunit

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
    lines |> Seq.map parseGame |> Seq.filter validGame |> Seq.sumBy (_.Id)

let solve2 lines =
    lines |> Seq.map parseGame |> Seq.map maxColor |> Seq.sum


let example =
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        .Split(Environment.NewLine)

[<Fact>]
let ``Part 1 (example)``() =
    let result = solve example

    Assert.Equal(8, result)

[<Fact>]
let ``Part 1``() =
    let txt = File.ReadLines(Path.Combine("Input", "day2.txt"))

    let result = solve txt

    Assert.Equal(2285, result)

[<Fact>]
let ``Part 2 (example)``() =
    let result = solve2 example

    Assert.Equal(2286, result)

[<Fact>]
let ``Part 2``() =
    let txt = File.ReadLines(Path.Combine("Input", "day2.txt"))

    let result = solve2 txt

    Assert.Equal(77021, result)
