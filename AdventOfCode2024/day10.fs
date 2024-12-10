module AdventOfCode2024.day10

open FSharpPlus
open System.IO
open AdventOfCode2024.Utils

let step (pos: Vec) (grid: Grid<int8>) =
    Vec.Cardinals |> Seq.map ((+) pos) |> Seq.filter grid.HasIndex

let solve procStep =
    let lines =
        File.ReadAllLines "day10.txt"
        |> Array.map (String.toArray >> Array.map (fun x -> x - '0' |> int8))

    let grid = { Data = lines }
    grid
    |> Grid.enumerate
    |> Seq.filter (snd >> is (int8 0))
    |> Seq.map (fun (pos, _) ->
        ([ pos ], seq { (int8 1)..(int8 9) })
        ||> Seq.fold (fun set height ->
            set
            |> Seq.collect (fun node -> grid |> step node)
            |> Seq.filter (fun next -> grid[next] = height)
            |> procStep
            |> Seq.toList)
        |> List.length)
    |> Seq.sum
    |> printfn "%d"
    

let run1() = solve distinct
let run2() = solve id
