open System.IO

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

        { Value = value
          Right = right
          Left = left })

let navigate node nodes direction =
    let value =
        match direction with
        | Left -> node.Left
        | Right -> node.Right

    nodes |> Array.find (fun n -> n.Value = value)

let rec traverse (node: Node) (nodes: Node array) (directions: Direction seq) (count: int) =
    if node.Value = "ZZZ" then
        count
    else
        let direction = directions |> Seq.head
        printfn $"%s{node.Value} -> %A{direction}"
        let nextNode = navigate node nodes direction
        printfn $"Next node is %s{nextNode.Value}"
        traverse nextNode nodes (directions |> Seq.tail) (count + 1)

let example =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

let lines = File.ReadAllLines(Path.Combine("2023", "Input", "day8.txt"))

// let lines = input.Split("\n")
let directions = lines[0] |> makeInstructions

let nodes = lines[2..] |> makeNodes

let generateDirections index =
    // let directions = [ Left; Left; Right ]
    directions[index % directions.Length]

let generator = Seq.initInfinite generateDirections

// generator |> Seq.take 20 |> Seq.toList |> List.iter (printfn "%A")

let firstNode = nodes |> Array.head
traverse firstNode nodes generator 0
