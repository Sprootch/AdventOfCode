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

let rec calculateDirectorySize (directory: Directory) : int =
    let fileSize = calcFileSize directory.Files
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
