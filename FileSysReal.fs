// ==============================================
// Tree implementation
// ==============================================
type Tree<'LeafData, 'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

module Tree =

    let rec cata fLeaf fNode (tree: Tree<'LeafData, 'INodeData>): 'r =
        let recurse = cata fLeaf fNode
        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) -> fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree: Tree<'LeafData, 'INodeData>): 'r =
        let recurse = fold fLeaf fNode
        match tree with
        | LeafNode leafInfo -> fLeaf acc leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let newAcc = fNode acc nodeInfo
            subtrees |> Seq.fold recurse newAcc

    let rec map fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) =
        let recurse = map fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = subtrees |> Seq.map recurse
            InternalNode(newNodeInfo, newSubtrees)

    let rec iter fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) =
        let recurse = iter fLeaf fNode
        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            subtrees |> Seq.iter recurse
            fNode nodeInfo

// ==============================================
// IO FileSystem as Tree
// ==============================================
module IOFileSystem_Tree =

    open System
    open System.IO

    type FileSystemTree = Tree<IO.FileInfo, IO.DirectoryInfo>

    let fromFile (fileInfo: FileInfo) = LeafNode fileInfo

    let rec fromDir (dirInfo: DirectoryInfo) =
        let subitems =
            seq {
                yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
                yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
        }
        InternalNode(dirInfo, subitems)

    let totalSize fileSystemItem =
        let fLeaf acc (leafInfo: FileInfo) = acc + leafInfo.Length
        let fNode acc (nodeInfo: DirectoryInfo) = acc
        Tree.fold fLeaf fNode 0L fileSystemItem

    let largestFile fileSystemItem =
        let fLeaf (largestSoFar: FileInfo option) (leafInfo: FileInfo) =
            match largestSoFar with
            | None -> Some leafInfo
            | Some largestfile -> if leafInfo.Length > largestfile.Length then Some leafInfo else largestSoFar

        let fNode largestSoFar nodeInfo = largestSoFar
        Tree.fold fLeaf fNode None fileSystemItem

    let dirListing fileSystemItem =
        let printDate (dt: DateTime) = dt.ToString()

        let fLeaf (leafInfo: FileInfo) =
            sprintf "%10d %s %s" leafInfo.Length (printDate leafInfo.LastWriteTime) leafInfo.Name

        let fNode (nodeInfo: DirectoryInfo) = nodeInfo.FullName
        Tree.map fLeaf fNode fileSystemItem

// ==============================================
// Parallel grep implementation
// ==============================================
module ParallelGrep =

    open System
    open System.IO
    open IOFileSystem_Tree

    let foldLinesAsync folder acc (fi:FileInfo) =
        async {
            let mutable acc = acc
            let mutable lineNo = 1
            use sr = new StreamReader(path=fi.FullName)
            while not sr.EndOfStream do
                let! lineText = sr.ReadLineAsync() |> Async.AwaitTask
                acc <- folder acc lineNo lineText
                lineNo <- lineNo + 1
            return acc
        }
    
    let asyncMap f asyncX =
        async {
            let! x = asyncX
            return (f x)
        }
    
    let matchPattern textPattern (fi:FileInfo) =
        let regex = Text.RegularExpressions.Regex(pattern=textPattern)
        let folder results lineNo lineText =
            if regex.IsMatch lineText then
                let result = sprintf "%40s:%-5i %s" fi.Name lineNo lineText
                result :: results
            else
                results
        fi
        |> foldLinesAsync folder []
        |> asyncMap List.rev

    let grep filePattern textPattern fileSystemItem =
        let regex = Text.RegularExpressions.Regex(pattern=filePattern)
        let matchFile (fi:FileInfo) =
            if regex.IsMatch fi.Name then
                Some (matchPattern textPattern fi)
            else
                None
        let fFile asyncs (fi:FileInfo) =
            (matchFile fi) :: asyncs
        let fDir asyncs (di:DirectoryInfo) =
            asyncs
        fileSystemItem
        |> Tree.fold fFile fDir []
        |> Seq.choose id
        |> Async.Parallel
        |> asyncMap (Array.toList >> List.collect id)

// Tests
open System
open System.IO
open IOFileSystem_Tree
open ParallelGrep

[<EntryPoint>]
let main argv =
    
    Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
    let currentDir = fromDir (DirectoryInfo("."))

    // printfn "Current dir size = %d" (totalSize currentDir)
    // let largestfile = largestFile currentDir
    // match largestfile with
    // | None -> printfn "Dir void"
    // | Some file -> printfn "Largest file : name = %s size = %d" file.Name file.Length
    // currentDir
    // |> dirListing
    // |> Tree.iter (printfn "%s") (printfn "%s")

    let grepdata =
        currentDir 
        |> grep "fs" "LinkedList" 
        |> Async.RunSynchronously
    grepdata
    |> List.iter (printfn "%s")
    0
