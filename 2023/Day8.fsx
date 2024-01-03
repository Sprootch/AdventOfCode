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

let navigate node nodes direction =
    let value =
        match direction with
        | Left -> node.Left
        | Right -> node.Right

    nodes |> List.find (fun n -> n.Value = value)

let rec traverse (node: Node) (nodes: Node list) (directions: Direction seq) (count: int) =
    if node.Value = "ZZZ" then
        count
    else
        let direction = directions |> Seq.head
        // printfn $"%s{node.Value} -> %A{direction}"
        let nextNode = navigate node nodes direction
        // printfn $"Next node is %s{nextNode.Value}"
        traverse nextNode nodes (directions |> Seq.tail) (count + 1)

let example =
    """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

let lines = example.Split("\n")
lines[0] |> makeInstructions

// lines[2..] |> makeNodes

let zzz =
    { Value = "ZZZ"
      Left = "ZZZ"
      Right = "ZZZ" }

let aaa =
    { Value = "AAA"
      Right = "BBB"
      Left = "BBB" }

let bbb =
    { Value = "BBB"
      Left = "AAA"
      Right = "ZZZ" }

let nodes = [ aaa; bbb; zzz ]

let generateDirections index =
    let directions = [ Left; Left; Right ]
    directions[index % directions.Length]

let generator = Seq.initInfinite generateDirections

// generator |> Seq.take 20 |> Seq.toList |> List.iter (printfn "%A")

let firstNode = nodes |> List.head
traverse firstNode nodes generator 0
