module Tree

type Tree<'LeafData, 'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

type FileInfo = { Name: string; FileSize: int }
type DirectoryInfo = { Name: string; DirSize: int }

type FileSystemItem = Tree<FileInfo, DirectoryInfo>

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

let fromFile (fileInfo: FileInfo) = LeafNode fileInfo

let fromDir (dirInfo: DirectoryInfo) subitems = InternalNode(dirInfo, subitems)

// Tests
let readme =
    fromFile { Name = "readme.txt"; FileSize = 1 }

let config =
    fromFile { Name = "config.xml"; FileSize = 2 }

let build =
    fromFile { Name = "build.bat"; FileSize = 3 }

let src =
    fromDir { Name = "src"; DirSize = 10 } [ readme; config; build ]

let bin =
    fromDir { Name = "bin"; DirSize = 10 } []

let root =
    fromDir { Name = "root"; DirSize = 5 } [ src; bin ]
