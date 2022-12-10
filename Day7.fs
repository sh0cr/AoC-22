module Day7

open System.Collections.Generic

type Dir = {
    Name: string;
    Parent: Dir option;
    Files: Dictionary<string, int>;
    Directories: Dictionary<string, Dir>
}

let mutable totalSum = 0
let limit = 100000

let rec traverseAndCompute (dir:Dir, sizes: List<int>) =
    let filesSize = Seq.sumBy (fun v -> v) dir.Files.Values
    let dirsSize = Seq.sumBy (fun d -> traverseAndCompute(d, sizes)) dir.Directories.Values
    let total = filesSize + dirsSize
    //if total <= limit then totalSum <- totalSum + total
    sizes.Add total
    total

let cleanup = 
    let lines = Utils.getLines 7
    let root:Dir = { Name= "/"; Files = new Dictionary<string,int>(); Directories = new Dictionary<string, Dir>(); Parent = None; }
    let mutable currentDir = root
    
    let parseLine (line: string) =
        let args = line.Split(' ')
        match args[0] with
        | "$" -> 
            match args[1] with
            | "cd" ->
                match args[2] with
                | "/" -> currentDir <- root
                | ".." -> 
                    currentDir <- currentDir.Parent.Value
                | _ -> currentDir <- currentDir.Directories[args[2]]
            | "ls" -> ()
            | _ -> invalidArg args[1] "unknown command"
        | "dir" -> currentDir.Directories.Add(args[1],
            {Name = args[1]; Files = new Dictionary<string, int>(); Directories = new Dictionary<string, Dir>(); Parent = Some(currentDir);})
        | size -> currentDir.Files.Add(args[1], size |> int)
    
    Seq.iter (fun x -> parseLine x) lines
    let sizes = new List<int>()
    let needToFree = 30000000 + traverseAndCompute(root, sizes) - 70000000
    sizes
    |> Seq.toList
    |> List.sort
    |> List.find (fun x -> x >= needToFree)