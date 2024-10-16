open System.IO

File.ReadLines(Path.Combine("2021/Input", "day1.txt"))
|> Seq.map int
|> Seq.pairwise
|> Seq.map (fun (current, next) -> next > current)
|> Seq.countBy id
