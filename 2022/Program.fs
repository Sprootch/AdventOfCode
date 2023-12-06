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
    { directory with Directories = (newDir :: directory.Directories) }

let addFile (file: File) (directory: Directory) =
    { directory with Files = (file :: directory.Files) }

let calcFileSize files = files |> List.sumBy (fun f -> f.Size)

let calculateDirectorySize directory =
    let rec innerCalc directories size =
        printfn $"Size: {size}"

        match directories with
        | [] -> size
        | head :: tail ->
            let value = head.Files |> calcFileSize
            printfn $"{head.Name}: Size = {value}"
            (innerCalc head.Directories (value + size)) + (innerCalc tail size) 

    innerCalc directory.Directories (calcFileSize directory.Files)

let e =
    { Name = "e"
      Files = [ { Name = "i"; Size = 584 } ]
      Directories = [] }

let a =
    { Name = "a"
      Directories = [ e ]
      Files =
        [ { Name = "f"; Size = 29116 }
          { Name = "g"; Size = 2557 }
          { Name = "h.lst"; Size = 62596 } ] }

let d =
    { Name = "d"
      Directories = []
      Files =
        [ { Name = "j"; Size = 4060174 }
          { Name = "d.log"; Size = 8033020 }
          { Name = "d.ext"; Size = 5626152 }
          { Name = "k"; Size = 7214296 } ] }

let root =
    { Name = "/"
      Directories = [ a; d ]
      Files = [ { Name = "b.txt"; Size = 14848514 }; { Name = "c.dat"; Size = 8504156 } ] }

// current 48380581
// expected 48381165

e |> calculateDirectorySize
a |> calculateDirectorySize
d |> calculateDirectorySize
root |> calculateDirectorySize
