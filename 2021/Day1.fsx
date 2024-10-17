open System.IO

File.ReadLines(Path.Combine("2021/Input", "day1.txt"))
|> Seq.map int
|> Seq.pairwise
|> Seq.map (fun (current, next) -> if next > current then 1 else 0)
|> Seq.sum

module Array =
    let pairwise3 (array: array<'a>) =
        seq {
            for i in 0 .. array.Length - 3 do
                yield (array[i], array[i + 1], array[i + 2])
        } |> Seq.toArray

File.ReadLines(Path.Combine("2021/Input", "day1_2.txt"))
|> Seq.map int
|> Seq.toArray
|> Array.pairwise3
|> Array.map (fun (a, b, c) -> a + b + c)
|> Array.pairwise
|> Array.map (fun (current, next) -> if next > current then 1 else 0)
|> Array.sum
