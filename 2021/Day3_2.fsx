open System.IO

let lines = File.ReadLines(Path.Combine("2021/Input", "day3_2.txt"))

let getColumn idx (list: char list list) = seq { for item in list -> item[idx] }

let toDecimal (chars: char list) =
    chars
    |> System.String.Concat
    |> (fun binary -> System.Convert.ToInt32(binary, 2))

let compute selector bytes =
    let get char seq =
        seq |> Seq.find (fun c -> fst c = char) |> snd

    let rec innerCompute index bytes =
        let countBy = bytes |> getColumn index |> Seq.countBy id

        let winner = selector (countBy |> get '0') (countBy |> get '1')
        let filtered = bytes |> List.filter (fun c -> c[index] = winner)

        if (filtered.Length = 1) then
            filtered
        else
            filtered |> innerCompute (index + 1)

    bytes |> innerCompute 0

let computeOxygen =
    compute (fun zeroes ones -> if (zeroes > ones) then '0' else '1')

let computeNitro = compute (fun zeroes ones -> if (ones < zeroes) then '1' else '0')

let bytes = [ for line in lines -> [ for bit in line -> bit ] ]
let oxygen = bytes |> computeOxygen |> List.head |> toDecimal
let nitroxyde = bytes |> computeNitro |> List.head |> toDecimal
oxygen * nitroxyde
