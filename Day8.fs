module Day8

let treeHouse =
    let grid =
        Utils.getLines 8
        |> Seq.map (fun s -> s.ToCharArray() |> Seq.map (fun c -> c |> int) |> Seq.toArray)
        |> array2D

    let w = Array2D.length1 grid
    let h = Array2D.length2 grid

    let isVisible i j height =
        let isTallest trees = 
            match Array.tryFind (fun x -> x >= height) trees with
            | Some(k) -> false
            | None -> true

        let isEdge = i = 0 || j = 0 || i = w - 1 || j = h - 1

        isEdge ||
        isTallest grid[i, 0 .. j - 1] ||
        isTallest grid[i, j + 1 ..] ||
        isTallest grid[0 .. i - 1, j] ||
        isTallest grid[i + 1 .., j]


    let scienicScore i j height =
        let getDist trees =
            let index = trees |> Array.tryFindIndex (fun x -> x >= height)

            match index with
            | Some(k) -> k + 1
            | None -> trees.Length

        (getDist (Array.rev grid[0 .. i - 1, j]))
        * (getDist grid[i + 1 .., j])
        * (getDist (Array.rev grid[i, 0 .. j - 1]))
        * (getDist grid[i, j + 1 ..])

    let mutable count = 0
    let mutable max = 0

    Array2D.iteri
        (fun i j x ->
            let score = scienicScore i j x
            if score > max then
                max <- score)
        grid

    max