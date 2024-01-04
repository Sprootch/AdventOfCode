open System.IO
open System.Text.RegularExpressions

type Node =
    { Value: string
      Left: string
      Right: string }

type Direction =
    | Left
    | Right

let makeInstructions(line: string) : Direction list =
    line
    |> Seq.map (function
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwith "invalid direction")
    |> Seq.toList

let makeNodes(lines: string array) =
    lines
    |> Array.map (fun line ->
        let value = line[0..2]
        let left = line[7..9]
        let right = line[12..14]

        (value,
         { Value = value
           Right = right
           Left = left }))
    |> Map.ofArray

let navigate node nodes direction =
    let value =
        match direction with
        | Left -> node.Left
        | Right -> node.Right

    nodes |> Map.find value

let rec traverse node nodes directions count =
    if node.Value = "ZZZ" then
        count
    else
        let direction = directions |> Seq.head
        // printfn $"%s{node.Value} -> %A{direction}"
        let nextNode = navigate node nodes direction
        // printfn $"Next node is %s{nextNode.Value}"
        traverse nextNode nodes (directions |> Seq.tail) (count + 1)

let example =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

let lines = File.ReadAllLines(Path.Combine("2023", "Input", "day8.txt"))

let directions = lines[0] |> makeInstructions

let nodes = lines[2..] |> makeNodes

let generator =
    Seq.initInfinite (fun index -> directions[index % directions.Length])

let firstNode = nodes["AAA"]
traverse firstNode nodes generator 0

let regex = @"\((?<value>\w{3})\) = \((?<left>\w{3}), (?<right>\w{3})\)";;
Regex.Match("BBB = (AAA, ZZZ)", regex)

Regex.Match("BBB = (AAA, ZZZ)", @"(\w{3}) = \(\w{3}, \w{3}\)")
