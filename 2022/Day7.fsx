open System
open System.IO

type File = { Name: string; Size: int }

type Directory =
    { Name: string
      Directories: Directory list
      Files: File list }

let newDirectory name =
    { Name = name
      Directories = []
      Files = [] }

let newFile name size = { Name = name; Size = size }

let addDirectory (newDir: Directory) (directory: Directory) =
    { directory with
        Directories = (newDir :: directory.Directories) }

let addFile (file: File) (directory: Directory) =
    { directory with
        Files = (file :: directory.Files) }

let isInt (str:string) =
  let isInt, _ = Int32.TryParse str
  isInt

let processCommand (command:string) =
    match command.Split(' ') with
    | [| "$"; args |] ->
      printfn $"command : %s{args}"
    | [| "$"; args ; param |] ->
      printfn $"command : %s{args} %s{param}"
    | [| "dir"; directory |] ->
      printfn $"dir : %s{directory}"
    | [| size; file |] when (size |> isInt) ->
      printfn $"file: %s{file} (%s{size})"
    | other ->
      printfn "%A" other

let txt = File.ReadLines(Path.Combine("2022/Input", "day7.txt"))
txt |> Seq.iter processCommand

let rec calculateDirectorySize (directory: Directory) : int =
    let fileSize = directory.Files |> List.sumBy (_.Size)
    let subDirSize = directory.Directories |> List.sumBy calculateDirectorySize
    subDirSize + fileSize

let e =
    { Name = "e"
      Files = [ { Name = "i"; Size = 584 } ]
      Directories = [] }

e |> calculateDirectorySize

let a =
    { Name = "a"
      Directories = [ e ]
      Files =
        [ { Name = "f"; Size = 29116 }
          { Name = "g"; Size = 2557 }
          { Name = "h.lst"; Size = 62596 } ] }

a |> calculateDirectorySize

let d =
    { Name = "d"
      Directories = []
      Files =
        [ { Name = "j"; Size = 4060174 }
          { Name = "d.log"; Size = 8033020 }
          { Name = "d.ext"; Size = 5626152 }
          { Name = "k"; Size = 7214296 } ] }

d |> calculateDirectorySize

let root =
    { Name = "/"
      Directories = [ a; d ]
      Files = [ { Name = "b.txt"; Size = 14848514 }; { Name = "c.dat"; Size = 8504156 } ] }

root |> calculateDirectorySize

[ a; e; d; root ]
|> List.map calculateDirectorySize
|> List.filter (fun size -> size <= 100000)
|> List.sum
