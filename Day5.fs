module Day5

open System
open System.Collections.Generic
open System.Linq

let parseInitState (allLines: string list) =
    let stackLines = 
        allLines
        |> Seq.takeWhile (fun s -> not (s.Contains('1')))
        |> Seq.toList
    let rowCount = Seq.length stackLines
    let columnCount = allLines[rowCount].Split(' ', StringSplitOptions.RemoveEmptyEntries).Count()
    let stacks = new List<Stack<char>>(columnCount)
    for i = 0 to columnCount-1 do
        stacks.Add(new Stack<char>())
        for j = rowCount-1 downto 0 do
            match stackLines[j][1+4*i] with
            | c when Char.IsLetter(c) -> stacks.[i].Push(c)
            | _ -> ()
    stacks, columnCount, rowCount

let parseMove (s: string) =
    let splited = s.Split(' ')
    (splited[1] |> int, splited[3] |> int, splited[5] |> int)

let supplyStacks = 
    let lines = Utils.getLines 5 |> Seq.toList
    let (stack, w,h) = parseInitState lines
    let moves = lines[w+1..]
    
    for m in moves do
        let (count, fromStack, toStack) = parseMove m
        let moved = new List<char>()
        for i = 0 to count-1 do
            moved.Add(stack.[fromStack-1].Pop())
        moved
        |> Seq.toList
        |> List.rev
        |> List.map (fun x -> stack.[toStack-1].Push(x))

    stack |> Seq.map (fun l -> l.Peek())