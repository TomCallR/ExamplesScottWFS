module FileSys

type FileSystemItem =
    | File of File
    | Directory of Directory

and File = { Name: string; FileSize: int }

and Directory =
    { Name: string
      DirSize: int
      Subitems: FileSystemItem list }

let rec cataFS fFile fDir item: 'r =
    let recurse = cataFS fFile fDir
    match item with
    | File file -> fFile file
    | Directory dir ->
        let resRecurse = dir.Subitems |> List.map recurse
        fDir (dir.Name, dir.DirSize, resRecurse)

let totalSize fileSystemItem =
    let fFile file = file.FileSize
    let fDir (name, dirsize, subsizes) = (List.sum subsizes) + dirsize
    cataFS fFile fDir fileSystemItem

let largestFile fileSystemItem =
    let ifNone defaultVal option = defaultArg option defaultVal

    let fileSize fileOpt =
        fileOpt
        |> Option.map (fun file -> file.FileSize)
        |> ifNone 0

    let fFile file = Some file

    let fDir (name, dirsize, subfiles) =
        match subfiles with
        | [] -> None
        | subfiles -> subfiles |> List.maxBy fileSize

    cataFS fFile fDir fileSystemItem

// Fold
let rec foldFS fFile fDir acc item: 'r =
    let recurse = foldFS fFile fDir
    match item with
    | File file -> fFile acc file
    | Directory dir ->
        let newacc = fDir acc (dir.Name, dir.DirSize)
        dir.Subitems |> List.fold recurse newacc

let totalSizeFold item =
    let fFile acc (file: File) = acc + file.FileSize
    let fDir acc (dirname, dirsize) = acc + dirsize
    foldFS fFile fDir 0 item

let largestFileFold item =
    let fFile (acc: File option) (file: File) =
        match acc with
        | None -> Some file
        | Some facc ->
            match facc.FileSize < file.FileSize with
            | true -> Some file
            | false -> Some facc

    let fDir acc (dirname, dirsize) = acc
    foldFS fFile fDir None item

// Test
let readme =
    File { Name = "readme.txt"; FileSize = 1 }

let config =
    File { Name = "config.xml"; FileSize = 2 }

let build =
    File { Name = "build.bat"; FileSize = 3 }

let src =
    Directory
        { Name = "src"
          DirSize = 10
          Subitems = [ readme; config; build ] }

let bin =
    Directory
        { Name = "bin"
          DirSize = 10
          Subitems = [] }

let root =
    Directory
        { Name = "root"
          DirSize = 5
          Subitems = [ src; bin ] }

// Tests totalSize et totalSizeFold
printfn "%i" (readme |> totalSize) // 1
printfn "%i" (src |> totalSize) // 16 = 10 + (1 + 2 + 3)
printfn "%i" (root |> totalSize) // 31 = 5 + 16 + 10

printfn "%i" (readme |> totalSizeFold) // 1
printfn "%i" (src |> totalSizeFold) // 16 = 10 + (1 + 2 + 3)
printfn "%i" (root |> totalSizeFold) // 31 = 5 + 16 + 10

// Tests largestFile et largestFileFold
printfn "%A" (root |> largestFile)
printfn "%A" (bin |> largestFile)
printfn "%A" (root |> largestFileFold)
printfn "%A" (bin |> largestFileFold)
