module Day9

open System.Collections.Generic

let rope =
    let lines = Utils.getLines 9
    let visited = new HashSet<int * int>()
    let start = (0,0)
    let knots =  new List<int*int>(List.init 10 (fun k -> start))

    let moveTail tl hd  = 
        let (t1, t2: int) = tl
        match fst hd - t1, snd hd - t2 with
        | 2,0  -> t1 + 1, t2
        | -2,0 -> t1 - 1, t2
        | 0,2 -> t1, t2 + 1
        | 0,-2 -> t1, t2 - 1 
        | 2, 1 | 1, 2 | 2, 2 -> t1 + 1, t2 + 1
        | 2, -1 | 1, -2 | 2, -2 -> t1 + 1, t2 - 1
        | -2, 1 | -1, 2 | -2, 2  -> t1 - 1, t2 + 1
        | -2, -1 | -1, -2 | -2, -2 -> t1 - 1, t2 - 1
        | _ -> tl
    
    visited.Add start |> ignore

    let step (hx,hy) direction =
        match direction with
        | "L" -> hx - 1, hy
        | "R" -> hx + 1, hy
        | "D" -> hx, hy - 1
        | "U" -> hx, hy + 1
        | _ -> invalidArg direction "unknown direction"

    let move (s:string) = 
        let args = s.Split(' ')
        let dist = args[1] |> int
        
        for i = 1 to dist do
            knots[0] <- step knots[0] args[0]
            for k = 1 to knots.Count - 1 do
                knots[k] <- moveTail knots[k] knots[k-1]
            visited.Add knots[knots.Count - 1] |> ignore

    Seq.iter move lines
    visited.Count