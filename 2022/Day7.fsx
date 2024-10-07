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

let calcFileSize files = files |> List.sumBy (_.Size)

let rec calcDirSize size (directory: Directory) =
    printfn "Dir is %s" directory.Name
    directory.Files |> List.iter (fun f -> printfn "%s : %d" f.Name f.Size)
    let fSize = calcFileSize directory.Files
    printfn "%d" fSize
    let newSize = size + fSize
    match directory.Directories with
    | [] ->
        printfn "no dir"
        newSize
        // size + (calcFileSize directory.Files)
    | head :: tail ->
        printfn "Child dir is %s" head.Name
        let xx = [ for i in tail do calcDirSize 0 i]
        let a = xx |> List.sum
        printfn "a = %d" a
        let x = calcDirSize newSize head
        x + a

let e =
    { Name = "e"
      Files = [ { Name = "i"; Size = 584 } ]
      Directories = [] }

// e |> calculateDirectorySize
e |> calcDirSize 0

let a =
    { Name = "a"
      Directories = [ e ]
      Files =
        [ { Name = "f"; Size = 29116 }
          { Name = "g"; Size = 2557 }
          { Name = "h.lst"; Size = 62596 } ] }

// a |> calculateDirectorySize
a |> calcDirSize 0

let d =
    { Name = "d"
      Directories = []
      Files =
        [ { Name = "j"; Size = 4060174 }
          { Name = "d.log"; Size = 8033020 }
          { Name = "d.ext"; Size = 5626152 }
          { Name = "k"; Size = 7214296 } ] }

d |> calcDirSize 0
// d |> calculateDirectorySize


let root =
    { Name = "/"
      Directories = [ a; d ]
      Files = [ { Name = "b.txt"; Size = 14848514 }; { Name = "c.dat"; Size = 8504156 } ] }

root |> calcDirSize 0
// current 48380581
// expected 48381165

// root |> calculateDirectorySize
