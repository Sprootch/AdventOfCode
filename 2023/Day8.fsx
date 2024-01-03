type Node =
    { Left: Node option
      Right: Node option
      Value: string }

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
    { Value = "AAA"
      Left = None
      Right = None }

let navigate node =
    function
    | Left -> node.Left
    | Right -> node.Right

let rec traverse (node: Node) (directions: Direction seq) (count:int) =
    if node.Value = "ZZZ" then count
    else
        let direction = directions |> Seq.head
        let nextNode = navigate node direction
        match nextNode with
        | None -> count
        | Some n -> traverse n (directions |> Seq.tail) (count+1)
    
let solve input = 0

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

lines[2..] |> makeNodes

let zzz =
    { Value = "ZZZ"
      Left = None
      Right = None }

let ggg =
    { Value = "GGG"
      Left = None
      Right = None }

let ccc =
    { Value = "CCC"
      Left = Some zzz
      Right = Some ggg }

let bbb =
    { Value = "BBB"
      Left = Some ggg
      Right = Some ggg }

let aaa =
    { Value = "AAA"
      Left = Some bbb
      Right = Some ccc }

let nodes = [ aaa; bbb; ccc; ggg; zzz ]

let generateDirections index =
    let directions = [ Left; Left; Right; ]
    directions[index % directions.Length]

let generator = Seq.initInfinite generateDirections
    
// generator |> Seq.take 20 |> Seq.toList |> List.iter (printfn "%A") 

traverse (nodes |> List.head) generator 0


