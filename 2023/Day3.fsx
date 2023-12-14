module Day3

open System

type State =
    | Symbol of string
    | Digit of string
    | Empty

type Position = int * int

let parse =
    function
    | c when c |> Char.IsDigit -> Digit(c |> string)
    | c when c = '.' -> Empty
    | c -> Symbol(c |> string)

let digit(Digit d) = d

type Bundled =
    | Empty
    | Number of int
    | Symbol of string

let isTouching (position: Position) ((numberPos, _): Position list * Bundled) =
    let x, y = position

    let c = numberPos |> Set.ofList

    let neighbours =
        [ for dx, dy in List.allPairs [ - 1 .. 1 ] [ - 1 .. + 1 ] do
              if (dx, dy) <> (0, 0) then
                  (dx + x, dy + y) ]
        |> Set.ofList

    Set.intersect c neighbours |> Set.isEmpty |> not

let hasSymbol (symbolPos: Position list) (number: Position list * Bundled) =
    symbolPos |> List.exists (fun pos -> isTouching pos number)

let rec bundle line =
    match line with
    | [] -> []
    | (_, Digit _) :: t ->
        let bundled =
            line
            |> List.takeWhile (fun (_, state) ->
                match state with
                | Digit _ -> true
                | _ -> false)

        let positions = bundled |> List.map fst
        let number = bundled |> List.map (snd >> digit) |> String.concat "" |> int |> Number

        let rest = line |> List.skip (bundled |> List.length)
        (positions, number) :: bundle rest
    | (pos, otherwise) :: t ->
        let entry =
            match otherwise with
            | State.Symbol s -> Bundled.Symbol s
            | State.Empty -> Bundled.Empty

        let c = [ pos ], entry
        c :: (bundle t)

let solve input =
    let lines input =
        [ for row, line in (input |> Seq.indexed) do
              [ for col, char in line |> Seq.indexed -> ((col, row), char |> parse) ] ]

    let bundled = lines input |> List.collect bundle

    let symbols =
        bundled
        |> List.filter (fun (_, b) ->
            match b with
            | Symbol _ -> true
            | _ -> false)
        |> List.collect fst

    let numbers =
        bundled
        |> List.filter (fun (_, b) ->
            match b with
            | Number _ -> true
            | _ -> false)

    let parts =
        numbers
        |> List.filter (hasSymbol symbols)
        |> List.map snd
        |> List.map (fun n ->
            match n with
            | Number n -> n)
        |> List.sum

    parts

let example =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

solve example

solve (System.IO.File.ReadAllLines @"C:\Projects\Perso\AdventOfCode\2023\Input\day3.txt")
