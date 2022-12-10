module Utils

open System.IO;

let filepath day =
    __SOURCE_DIRECTORY__ + @"\input\input" + $"{day}.txt"

let getString day =
    File.ReadAllText(filepath day)

let getLines day =
    File.ReadLines(filepath day)