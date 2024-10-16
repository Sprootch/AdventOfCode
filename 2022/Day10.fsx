open System.IO

let lines = File.ReadLines(Path.Combine("2022/Input", "day10.txt"))

type Cpu = { CycleCount: int; Register: int }

let scanner (cpu: Cpu) (line: string) =
    match line.Split(' ') with
    | [| "addx"; count |] ->
        let value = count |> int

        { cpu with
            CycleCount = cpu.CycleCount + 2
            Register = cpu.Register + value }
    | _ ->
        { cpu with
            CycleCount = cpu.CycleCount + 1 }

let cpu = { CycleCount = 0; Register = 1 }
let instructions = lines |> Seq.scan scanner cpu
instructions |> Seq.tryFind (fun c -> c.CycleCount = 20)
instructions |> Seq.tryFind (fun c -> c.CycleCount = 60)
instructions |> Seq.tryFind (fun c -> c.CycleCount = 100)
instructions |> Seq.tryFind (fun c -> c.CycleCount = 140)
instructions |> Seq.tryFind (fun c -> c.CycleCount = 180)
instructions |> Seq.tryFind (fun c -> c.CycleCount = 220)
// lines |> Seq.scan (fun l -> ) list.Empty
