open System
open System.IO

type DirCommand =
    | CreateDirectory of string
    | GoToDirectory of string
    | AddFile of (string * int)
    | GoToParentDirectory
    | Noop

type File = { Name: string; Size: int }

type Directory =
    { Name: string
      Parent: Directory option
      Directories: Directory list
      Files: File list }

let newDirectory name parent =
    { Name = name
      Parent = Some parent
      Directories = []
      Files = [] }

let newFile name size = { Name = name; Size = size }

let addDirectory (newDir: Directory) (directory: Directory) =
    { directory with
        Directories = (newDir :: directory.Directories) }

let addFile (file: string * int) (directory: Directory) =
    let name, size = file
    let f = { Name = name; Size = size }

    { directory with
        Files = (f :: directory.Files) }

let isInt (str: string) =
    let isInt, _ = Int32.TryParse str
    isInt

let execCommand (directory: Directory) =
    printfn "%A" directory
    function
    | Noop -> directory
    | GoToDirectory dirName ->
        match (directory.Directories |> List.tryFind (fun dir -> dir.Name = dirName)) with
        | Some d -> d
        | None -> failwith $"Directory not created {dirName}"
    | GoToParentDirectory ->
        match directory.Parent with
        | None -> directory
        | Some d -> d
    | CreateDirectory name ->
        printfn $"Create directory {name}"
        let newDir = newDirectory name directory
        directory |> addDirectory newDir
    | AddFile(name, size) ->
        printfn $"add file {name} to {directory.Name}"
        directory |> addFile (name, size)

let root =
    { Name = "/"
      Parent = None
      Directories = []
      Files = [] }

let parseCommand (command: string) =
    match command.Split(' ') with
    | [| "$"; "ls" |] -> Noop
    | [| "$"; "cd"; ".." |] -> GoToParentDirectory
    | [| "$"; "cd"; dirName |] -> GoToDirectory dirName
    | [| "dir"; dirName |] -> CreateDirectory dirName
    | [| size; file |] when (size |> isInt) -> AddFile(file, size |> int)
    | other ->
        printfn $"! %A{other}"
        Noop

let rec calculateDirectorySize (directory: Directory) : int =
    let fileSize = directory.Files |> List.sumBy (_.Size)
    let subDirSize = directory.Directories |> List.sumBy calculateDirectorySize
    subDirSize + fileSize

let txttest = ["$ ls";
"dir drblq";
"133789 fjf";
"dir jpfrhmw";
"dir jqfwd";
"dir ncgffsr";
"12962 ntnr.lrq";
"dir qnbq";
"dir rqdngnrq";
"dir shcvnqq";
"dir vsd";
"dir vtzvf";
"$ cd drblq";
"$ ls";
"133843 bglzqdd";
"dir brfnfhj";
"268201 fbqjmp.jzv";
"80676 shcvnqq";
"$ cd brfnfhj";
"$ ls" ]
// let txt = File.ReadLines(Path.Combine("2022/Input", "day7.txt"))
let lastParsed = txttest |> List.map parseCommand |> List.fold execCommand root
// let rooted = lastParsed.Parent

//
// let e =
//     { Name = "e"
//       Parent = None
//       Files = [ { Name = "i"; Size = 584 } ]
//       Directories = [] }
//
// e |> calculateDirectorySize
//
// let a =
//     { Name = "a"
//       Parent = None
//       Directories = [ e ]
//       Files =
//         [ { Name = "f"; Size = 29116 }
//           { Name = "g"; Size = 2557 }
//           { Name = "h.lst"; Size = 62596 } ] }
//
// a |> calculateDirectorySize
//
// let d =
//     { Name = "d"
//       Directories = []
//       Parent = None
//       Files =
//         [ { Name = "j"; Size = 4060174 }
//           { Name = "d.log"; Size = 8033020 }
//           { Name = "d.ext"; Size = 5626152 }
//           { Name = "k"; Size = 7214296 } ] }

// d |> calculateDirectorySize

// let root =
//     { Name = "/"
//       Parent = None
//       Directories = [ a; d ]
//       Files = [ { Name = "b.txt"; Size = 14848514 }; { Name = "c.dat"; Size = 8504156 } ] }

// root |> calculateDirectorySize
//
// [ a; e; d; root ]
// |> List.map calculateDirectorySize
// |> List.filter (fun size -> size <= 100000)
// |> List.sum
