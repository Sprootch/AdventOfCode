open System.IO

let mapper state (line: string) =
    let hpos, depth = state

    match line.Split(' ') with
    | [| move; strVal |] ->
        let value = (strVal |> int)

        match move with
        | "up" -> (hpos, depth - value)
        | "down" -> (hpos, depth + value)
        | "forward" -> (hpos + value, depth)
        | _ -> (hpos, depth)
    | _ -> (hpos, depth)

let hpos, depth =
    File.ReadLines(Path.Combine("2021/Input", "day2.txt")) |> Seq.fold mapper (0, 0)

hpos * depth

let mapper2 state (line: string) =
    let hpos, depth, aim = state

    match line.Split(' ') with
    | [| move; strVal |] ->
        let value = (strVal |> int)

        match move with
        | "up" -> (hpos, depth, aim - value)
        | "down" -> (hpos, depth, aim + value)
        | "forward" -> (hpos + value, depth + (aim * value), aim)
        | _ -> (hpos, depth, aim)
    | _ -> (hpos, depth, aim)

let hpos2, depth2, aim =
    File.ReadLines(Path.Combine("2021/Input", "day2_2.txt"))
    |> Seq.fold mapper2 (0, 0, 0)
hpos2 * depth2
