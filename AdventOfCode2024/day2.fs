module AdventOfCode2024.day2

open System
open System.IO
open FSharpPlus
open AdventOfCode2024.Utils

let isGentle: int list -> bool =
    Seq.pairwise
    >> Seq.map (fun (a, b) -> Math.Abs(a - b))
    >> Seq.forall (fun level -> level >= 1 && level <= 3)

let isAscending: int list -> bool = Seq.pairwise >> Seq.forall (fun (a, b) -> a > b)

let isDescending: int list -> bool =
    Seq.pairwise >> Seq.forall (fun (a, b) -> a < b)

let isSafe (list: int list) =
    isGentle list && (isAscending list || isDescending list)

let numbers () =
    let lines = File.ReadAllLines "./data2.txt"
    lines |> map (String.split [ " " ] >> Seq.map int >> toList)

let checkDampened (list: int list) (index: int) =
    (list[0 .. index - 1] @ list[index + 1 .. list.Length - 1]) |> isSafe

let run1 () =
    numbers () |> Seq.countIf isSafe |> printfn "Part1: %d"

let run2 () =
    numbers ()
    |> Seq.countIf (fun list -> isSafe list || Seq.init (list.Length - 1) id |> Seq.exists (checkDampened list))
    |> printfn "Part2: %d"
