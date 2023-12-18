module Day6

open System

type Race = { Time: int; Distance: int }

let example =
    """Time:      7  15   30
Distance:  9  40  200"""

let input =
    """Time:        40     82     84     92
Distance:   233   1011   1110   1487"""

let compute(race: Race) =
    [ for holdTime in 0 .. race.Time -> holdTime * (race.Time - holdTime) ]
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
        |> Array.map (fun (t, d) -> { Time = t |> int; Distance = d |> int })

    races |> Array.map compute |> Array.filter (fun w -> w <> 0) |> Array.reduce (*)

// solve example
solve input
