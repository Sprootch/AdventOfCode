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
    | [|"noop"|] ->
        { cpu with
            CycleCount = cpu.CycleCount + 1 }
    | _ -> cpu

let rec find cycleCount instructions =
    match instructions |> Seq.tryFind (fun c -> c.CycleCount = cycleCount) with
    | Some cpu -> cpu
    | None -> find (cycleCount - 1) instructions

let signalStrength cycleCount instructions =
    let cpu = instructions |> find (cycleCount-1)
    cpu.Register * cycleCount

let cpu = { CycleCount = 0; Register = 1 }
let instructions = lines |> Seq.scan scanner cpu

[ 20; 60; 100; 140; 180; 220 ]
|> List.map (fun cycle -> instructions |> signalStrength cycle)
|> List.sum
