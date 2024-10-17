open System.IO

let lines = File.ReadLines(Path.Combine("2021/Input", "day3.txt"))

let getColumn idx array =
    let j = array |> Array2D.length1
    seq { for i in 0 .. j - 1 -> array[i, idx] }

let bytes = array2D [ for line in lines -> [ for bit in line -> bit ] ]

let compute2 (pick: ('t -> 'u) -> 'x seq -> 'x) =
    let size = (bytes |> Array2D.length2) - 1

    [ for i in 0..size -> bytes |> getColumn i |> Seq.countBy id |> (pick snd) |> fst ]
    |> System.String.Concat
    |> (fun binary -> System.Convert.ToInt32(binary, 2))

let compute (pick: (char * int) seq -> char * 'a) =
    let size = (bytes |> Array2D.length2) - 1

    [ for i in 0..size -> bytes |> getColumn i |> Seq.countBy id |> pick |> fst ]
    |> System.String.Concat
    |> (fun binary -> System.Convert.ToInt32(binary, 2))

let gamma = compute2 Seq.maxBy
let epsilon = compute2 Seq.minBy
gamma * epsilon
