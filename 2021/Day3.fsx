open System.IO

let lines = File.ReadLines(Path.Combine("2021/Input", "day3.txt"))

let getColumn idx array =
    let j = array |> Array2D.length1
    // todo yield ?
    [| for i in 0 .. j - 1 -> array[i, idx] |]

let trees = array2D [ for line in lines -> [ for tree in line -> tree ] ]

let gamma =
    // todo 4
    [ for i in 0..4 -> trees |> getColumn i |> Array.countBy id |> Array.maxBy snd |> fst ]
    |> System.String.Concat
    |> (fun binary -> System.Convert.ToInt32(binary, 2))
