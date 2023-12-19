open System

type Race = { Time: int64; Distance: int64 }

let example =
    """Time:      71530
Distance:  940200"""

let input =
    """Time:        40828492
Distance:   233101111101487"""

let compute(race: Race) =
    [ for holdTime in 0L .. race.Time -> holdTime * (race.Time - holdTime) ]
    |> List.filter (fun distance -> distance > race.Distance)
    |> List.length

let solve(input: string) =
    let lines = input.Split("\n")

    let times =
        lines[0].Replace("Time:", "").Split(" ", StringSplitOptions.RemoveEmptyEntries)

    let distances =
        lines[1]
            .Replace("Distance:", "")
            .Split(" ", StringSplitOptions.RemoveEmptyEntries)

    let races =
        Array.zip times distances
        |> Array.map (fun (t, d) -> { Time = t |> int64; Distance = d |> int64 })

    races |> Array.map compute |> Array.filter (fun w -> w <> 0) |> Array.reduce (*)

// solve example
solve input
