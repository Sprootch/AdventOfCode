open System.IO

let mapper (hpos, depth) (str: string) : (int * int) =
    match str.Split(' ') with
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
