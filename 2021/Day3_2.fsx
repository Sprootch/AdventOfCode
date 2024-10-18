open System.IO

let lines = File.ReadLines(Path.Combine("2021/Input", "day3_2.txt"))

let getColumn idx (list: char list list) =
    seq { for item in list -> item[idx] } |> Seq.toList

let toDecimal (chars: char list) =
    chars
    |> System.String.Concat
    |> (fun binary -> System.Convert.ToInt32(binary, 2))

// todo merge
let rec computeOxygen index bytes =
    let get char seq =
        seq |> List.find (fun c -> fst c = char) |> snd

    let countBy = bytes |> getColumn index |> List.countBy id
    let zeros = countBy |> get '0'
    let ones = countBy |> get '1'

    let winner = if (zeros > ones) then '0' else '1'
    let n = bytes |> List.filter (fun c -> c[index] = winner)
    if (n.Length = 1) then n else n |> computeOxygen (index + 1)

let rec computeNitro index bytes =
    let get char seq =
        seq |> List.find (fun c -> fst c = char) |> snd

    let countBy = bytes |> getColumn index |> List.countBy id
    let zeros = countBy |> get '0'
    let ones = countBy |> get '1'

    let winner = if (ones < zeros) then '1' else '0'
    let n = bytes |> List.filter (fun c -> c[index] = winner)
    if (n.Length = 1) then n else n |> computeNitro (index + 1)

let bytes = [ for line in lines -> [ for bit in line -> bit ] ]
let oxygen = bytes |> computeOxygen 0 |> List.head |> toDecimal
let nitroxyde = bytes |> computeNitro 0 |> List.head |> toDecimal
oxygen * nitroxyde
